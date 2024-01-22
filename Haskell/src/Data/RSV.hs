{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralisedNewtypeDeriving, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeFamilies #-}

module Data.RSV (
  allTrueValues,
  allFalseValues,
  convertValue,
  decodeValue,
  defaultFalseValues,
  defaultTrueValues,
  en,
  encode,
  encodeBinary,
  encodeBinaryLazy,
  encodeBool,
  encodeNull,
  encodeRow,
  encodeShow,
  encodeStringUnsafe,
  encodeText,
  encodeValue,
  encodeWith,
  foldApp,
  mappendA,
  newParserState,
  nullChar,
  parse,
  parseBinary,
  parseBinaryLazy,
  parseBool,
  parseRead,
  parseRow,
  parseString,
  parseText,
  parseValue,
  parseWith,
  permitNull,
  rowTerminatorChar,
  throwIndexedException,
  valueTerminatorChar,
  Encodable(..),
  FromRow(..),
  FromValue(..),
  IndexedException,
  ParserException(..),
  ParserConfig(..),
  ParserIndices(..),
  ParseResult,
  ParserState(..),
  RowParser,
  ToRow(..),
  ToValue(..),
  ValueParser,
  (<+>)
) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Reader (asks, runReader, MonadReader(..), Reader)
import Control.Monad.State (gets, modify, MonadState(..))
import Control.Monad.Trans.RWS (evalRWST, runRWST, RWST)
import Data.Bifunctor
import Data.ByteString.Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString (StrictByteString)
import Data.Default
import Data.Scientific
import Data.Set (member, Set)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Word
import Text.Printf
import Text.Read hiding (get)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID

askGet :: (MonadReader env m, MonadState state m) => m (env, state)
askGet = liftA2 (,) ask get 

valueTerminatorChar, nullChar, rowTerminatorChar :: Word8
valueTerminatorChar = 0xFF
nullChar = 0xFE
rowTerminatorChar = 0xFD

data ParserIndices = ParserIndices {
  byteIndex  :: !Integer,
  valueIndex :: !Integer,
  rowIndex   :: !Integer
} deriving (Eq, Show)

advanceByteIndex :: ParserIndices -> ParserIndices
advanceByteIndex (ParserIndices b v r) = ParserIndices (b + 1) v r

data ParserState = ParserState {
  remainingBytes :: [Word8],
  parserIndices  :: !ParserIndices
} deriving (Eq, Show)

newParserState :: LazyByteString -> ParserState
newParserState lbs = ParserState (LB.unpack lbs) (ParserIndices 0 0 0)

advanceParserState :: [Word8] -> ParserState -> ParserState
advanceParserState bytes (ParserState _ indices) = ParserState bytes (advanceByteIndex indices)

data ParserConfig = ParserConfig {
  trueValue   :: !Text,
  falseValue  :: !Text,
  trueValues  :: !(Set Text),
  falseValues :: !(Set Text)
}

allTrueValues :: ParserConfig -> Set Text
allTrueValues ParserConfig{..} = trueValues <> Set.fromList [trueValue] 

allFalseValues :: ParserConfig -> Set Text
allFalseValues ParserConfig{..} = falseValues <> Set.fromList [falseValue]
 
defaultTrueValues :: Set Text
defaultTrueValues = Set.fromList ["t", "yes", "y", "1"]

defaultFalseValues :: Set Text
defaultFalseValues = Set.fromList ["f", "no", "n", "0"]

instance Default ParserConfig where
  def = ParserConfig "true" "false" defaultTrueValues defaultFalseValues 

data ParserException = UnknownError | UnexpectedEOF | UnexpectedNull | UnexpectedRowTerminator | UnpermittedNull | UnicodeError | ConversionError String | MissingRowTerminator deriving (Eq, Show)
type IndexedException = (ParserIndices, ParserException)

newtype ValueParser a = ValueParser (RWST ParserConfig () ParserState (Either IndexedException) a)
  deriving (Functor, Applicative, Monad, MonadReader ParserConfig, MonadState ParserState, MonadError IndexedException)

throwIndexedException :: (MonadState ParserState m, MonadError IndexedException m) => ParserException -> m a
throwIndexedException e = do
  indices <- gets parserIndices
  throwError (indices, e)

instance Alternative ValueParser where
  empty = throwIndexedException UnknownError
  (ValueParser lhs) <|> (ValueParser rhs) = ValueParser $ do
    (config, state) <- askGet
    let result = runRWST lhs config state
    case result of
      Left _ -> rhs
      Right (a, newState, _) -> put newState >> return a

decodeValue :: ParserState -> Either IndexedException (Maybe StrictByteString, ParserState)
decodeValue state = first toStrictByteString <$> decodeValue' (resetValueIndex state) mempty False
  where
    decodeValue' :: ParserState -> [Word8] -> Bool -> ParseResult ([Word8], ParserState)
    decodeValue' (ParserState [] indices) _ _ = throwError (indices, UnexpectedEOF)
    decodeValue' state@(ParserState (byte:remainingBytes) indices) accum hasNull
      | byte == rowTerminatorChar = throwError (indices, UnexpectedRowTerminator)
      | not (null accum) && byte == nullChar = throwError (indices, UnexpectedNull)
      | not hasNull && byte == nullChar = decodeValue' (advanceParserState remainingBytes state) [byte] True
      | hasNull && byte /= valueTerminatorChar = throwError (rewindByteIndex indices, UnexpectedNull)
      | byte == valueTerminatorChar = return (accum, advanceParserState remainingBytes state)
      | otherwise = decodeValue' (advanceParserState remainingBytes state) (accum <> [byte]) hasNull
    toStrictByteString :: [Word8] -> Maybe StrictByteString
    toStrictByteString bytes
      | bytes == [nullChar] = Nothing
      | otherwise = Just $ SB.pack bytes
    resetValueIndex :: ParserState -> ParserState
    resetValueIndex (ParserState bytes (ParserIndices b _ r)) = ParserState bytes (ParserIndices b b r)
    rewindByteIndex :: ParserIndices -> ParserIndices
    rewindByteIndex (ParserIndices b v r) = ParserIndices (b - 1) v r

parseValue :: ValueParser StrictByteString
parseValue = do
  result <- gets decodeValue
  case result of
    Left e -> throwError e
    Right (Nothing, newState) -> put newState >> throwIndexedException UnpermittedNull
    Right (Just bs, newState) -> put newState >> return bs

parseText :: ValueParser Text
parseText = do
  sbs <- parseValue
  case TE.decodeUtf8' sbs of
    Left _ -> throwIndexedException UnicodeError
    Right t -> return t

parseString :: ValueParser String
parseString = T.unpack <$> parseText

parseBinary :: ValueParser StrictByteString
parseBinary = parseValue >>= convertValue B64.decode

parseBinaryLazy :: ValueParser LazyByteString
parseBinaryLazy = LB.fromStrict <$> parseBinary

parseRead :: Read a => String -> ValueParser a
parseRead typename = do
  s <- parseString
  case readMaybe s of
    Nothing -> throwIndexedException $ ConversionError $ printf "Could not convert string %s to desired type %s." s typename
    Just a -> return a

parseBool :: ValueParser Bool
parseBool = do
    config <- ask
    parseText >>= convertValue (toBool config)
    where
      toBool :: ParserConfig -> Text -> Either String Bool
      toBool config text
        | lowerText `member` allTrueValues config = return True 
        | lowerText `member` allFalseValues config = return False
        | otherwise = throwError $ printf "Could not convert string %s to desired type Bool." (show text) 
        where
          lowerText = T.toLower text

parseList :: FromValue a => ValueParser [a]
parseList = do 
  done <- isAtRowTerminator
  if done
    then return []
    else liftA2 (:) fromValue parseList
  where
    isAtRowTerminator = gets remainingBytes >>= checkRowTerminator 
    checkRowTerminator [] = throwIndexedException UnexpectedEOF 
    checkRowTerminator (byte:_)
      | byte == rowTerminatorChar = return True
      | otherwise = return False

convertValue :: (v -> Either String a) -> v -> ValueParser a
convertValue convert value = case convert value of
  Left error -> throwIndexedException (ConversionError error)
  Right a -> return a

permitNull :: ValueParser a -> ValueParser (Maybe a)
permitNull parser = catchError (Just <$> parser) handleError
  where
    handleError (_, UnpermittedNull) = modify advanceBy2 >> return Nothing
    handleError e = throwError e
    advanceBy2 (ParserState bytes (ParserIndices b v r)) = ParserState (drop 2 bytes) (ParserIndices (b + 2) v r)

class FromValue a where
  fromValue :: ValueParser a

instance FromValue Text where
  fromValue = parseText

instance FromValue String where
  fromValue = parseString

instance FromValue Int where
  fromValue = parseRead "Int"

instance FromValue Integer where
  fromValue = parseRead "Integer"

instance FromValue Double where
  fromValue = parseRead "Double"

instance FromValue Scientific where
  fromValue = parseRead "Scientific"

instance FromValue Bool where
  fromValue = parseBool 

instance FromValue UUID where
  fromValue = parseString >>= convertValue toUUID
    where
      toUUID :: String -> Either String UUID
      toUUID s = case UUID.fromString s of
        Just uuid -> return uuid
        Nothing -> throwError $ printf "Could not convert %s to a UUID." s

instance FromValue StrictByteString where
  fromValue = parseBinary

instance FromValue LazyByteString where
  fromValue = parseBinaryLazy

instance FromValue a => FromValue (Maybe a) where
  fromValue = permitNull fromValue

newtype RowParser a = RowParser (RWST ParserConfig () ParserState (Either IndexedException) a)
  deriving (Functor, Applicative, Monad, MonadReader ParserConfig, MonadState ParserState, MonadError IndexedException)

instance Alternative RowParser where
  empty = throwIndexedException UnknownError
  (RowParser lhs) <|> (RowParser rhs) = RowParser $ do
    (config, state) <- askGet
    let result = runRWST lhs config state 
    case result of
      Left _ -> rhs
      Right (a, newState, _) -> put newState >> return a

parseRow :: ValueParser a -> RowParser a
parseRow (ValueParser parser) = do
  (config, state) <- askGet
  case runRWST parser config (resetRow state) of
    Left e -> throwError e
    Right (result, newState, _) -> put newState >> finish result (remainingBytes newState)
  where
    finish _ [] = throwIndexedException UnexpectedEOF
    finish a (byte:bytes)
      | byte == rowTerminatorChar = modify (advanceParserState bytes) >> return a
      | otherwise = throwIndexedException MissingRowTerminator
    resetRow (ParserState bytes indices) = ParserState bytes (resetRowIndex indices)
    resetRowIndex :: ParserIndices -> ParserIndices
    resetRowIndex (ParserIndices b _ _) = ParserIndices b b b

class FromRow a where
  fromRow :: RowParser a

instance (FromValue a, FromValue b) => FromRow (a, b) where
  fromRow = parseRow $ (,) <$> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c) => FromRow (a, b, c) where
  fromRow = parseRow $ (,,) <$> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d) => FromRow (a, b, c, d) where
  fromRow = parseRow $ (,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e) => FromRow (a, b, c, d, e) where
  fromRow = parseRow $ (,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f) => FromRow (a, b, c, d, e, f) where
  fromRow = parseRow $ (,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g) => FromRow (a, b, c, d, e, f, g) where
  fromRow = parseRow $ (,,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g, FromValue h) => FromRow (a, b, c, d, e, f, g, h) where
  fromRow = parseRow $ (,,,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g, FromValue h, FromValue i) => FromRow (a, b, c, d, e, f, g, h, i) where
  fromRow = parseRow $ (,,,,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue 

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g, FromValue h, FromValue i, FromValue j) => FromRow (a, b, c, d, e, f, g, h, i, j) where
  fromRow = parseRow $ (,,,,,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g, FromValue h, FromValue i, FromValue j, FromValue k) => FromRow (a, b, c, d, e, f, g, h, i, j, k) where
  fromRow = parseRow $ (,,,,,,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g, FromValue h, FromValue i, FromValue j, FromValue k, FromValue l) => FromRow (a, b, c, d, e, f, g, h, i, j, k, l) where
  fromRow = parseRow $ (,,,,,,,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g, FromValue h, FromValue i, FromValue j, FromValue k, FromValue l, FromValue m) => FromRow (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  fromRow = parseRow $ (,,,,,,,,,,,,) <$> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue <*> fromValue

instance FromValue a => FromRow [a] where
  fromRow = parseRow parseList

instance (FromValue a, Ord a) => FromRow (Set a) where
  fromRow = Set.fromList <$> fromRow 

type ParseResult a = Either IndexedException a

parseWith :: FromRow a => ParserConfig -> LazyByteString -> ParseResult [a] 
parseWith config lbs = fst <$> evalRWST (parse' fromRow) config (newParserState lbs)
  where
    parse' (RowParser parser) = do 
      isAtEnd <- atEnd
      if isAtEnd
        then return []
        else liftA2 (:) parser (parse' (RowParser parser))
    atEnd = gets $ null . remainingBytes

parse :: FromRow a => LazyByteString -> ParseResult [a] 
parse = parseWith def

type Encoder = Reader ParserConfig Builder

infixr 6 `mappendA`

mappendA :: (Applicative f, Monoid m) => f m -> f m -> f m 
mappendA = liftA2 (<>)

infixr 6 <+>

(<+>) :: (Applicative f, Monoid m) => f m -> f m -> f m 
a <+> b = mappendA a b 

foldApp :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldApp f = foldr (\a accum -> f a <+> accum) (pure mempty)

encodeValue :: Encoder -> Encoder 
encodeValue encoder = encoder <+> pure (word8 valueTerminatorChar)

encodeBuilder :: Builder -> Encoder
encodeBuilder = encodeValue . pure

encodeNull :: Encoder
encodeNull = encodeBuilder $ word8 nullChar 

encodeStringUnsafe :: String -> Encoder
encodeStringUnsafe = encodeBuilder . stringUtf8 

encodeText :: Text -> Encoder
encodeText = encodeStringUnsafe . T.unpack

encodeShow :: Show a => a -> Encoder 
encodeShow = encodeStringUnsafe . show

encodeBool :: Bool -> Encoder
encodeBool True = asks trueValue >>= encodeText 
encodeBool False = asks falseValue >>= encodeText

encodeBinary :: StrictByteString -> Encoder
encodeBinary = encodeBuilder . byteString . B64.encode

encodeBinaryLazy :: LazyByteString -> Encoder
encodeBinaryLazy = encodeBuilder . lazyByteString . B64L.encode

class ToValue a where
  toValue :: a -> Encoder

instance ToValue Text where
  toValue = encodeText

instance ToValue String where
  toValue = encodeStringUnsafe

instance ToValue Int where
  toValue = encodeShow

instance ToValue Integer where
  toValue = encodeShow

instance ToValue Double where
  toValue = encodeShow

instance ToValue Scientific where
  toValue = encodeShow

instance ToValue Bool where
  toValue = encodeBool

instance ToValue UUID where
  toValue = encodeStringUnsafe . UUID.toString

instance ToValue StrictByteString where
  toValue = encodeBinary

instance ToValue LazyByteString where
  toValue = encodeBinaryLazy

instance ToValue a => ToValue (Maybe a) where
  toValue Nothing = encodeNull
  toValue (Just a) = toValue a

encodeRow :: Encoder -> Encoder
encodeRow = (<+> pure (word8 rowTerminatorChar))

class ToRow a where
  toRow :: a -> Encoder

instance (Foldable t, ToValue a) => ToRow (t a) where
  toRow = encodeRow . foldApp toValue

data Encodable = forall a. ToValue a => Encodable !a

en :: ToValue a => a -> Encodable
en = Encodable

instance ToValue Encodable where
  toValue (Encodable a) = toValue a

instance {-# OVERLAPPING #-} (ToValue a, ToValue b) => ToRow (a, b) where
  toRow (a, b) = toRow [en a, en b] 

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c) => ToRow (a, b, c) where
  toRow (a, b, c) = toRow [en a, en b, en c] 

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d) => ToRow (a, b, c, d) where
  toRow (a, b, c, d) = toRow [en a, en b, en c, en d]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e) => ToRow (a, b, c, d, e) where
  toRow (a, b, c, d, e) = toRow [en a, en b, en c, en d, en e]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f) => ToRow (a, b, c, d, e, f) where
  toRow (a, b, c, d, e, f) = toRow [en a, en b, en c, en d, en e, en f]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g) => ToRow (a, b, c, d, e, f, g) where
  toRow (a, b, c, d, e, f, g) = toRow [en a, en b, en c, en d, en e, en f, en g]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g, ToValue h) => ToRow (a, b, c, d, e, f, g, h) where
  toRow (a, b, c, d, e, f, g, h) = toRow [en a, en b, en c, en d, en e, en f, en g, en h]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g, ToValue h, ToValue i) => ToRow (a, b, c, d, e, f, g, h, i) where
  toRow (a, b, c, d, e, f, g, h, i) = toRow [en a, en b, en c, en d, en e, en f, en g, en h, en i]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g, ToValue h, ToValue i, ToValue j) => ToRow (a, b, c, d, e, f, g, h, i, j) where
  toRow (a, b, c, d, e, f, g, h, i, j) = toRow [en a, en b, en c, en d, en e, en f, en g, en h, en i, en j]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g, ToValue h, ToValue i, ToValue j, ToValue k) => ToRow (a, b, c, d, e, f, g, h, i, j, k) where
  toRow (a, b, c, d, e, f, g, h, i, j, k) = toRow [en a, en b, en c, en d, en e, en f, en g, en h, en i, en j, en k]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g, ToValue h, ToValue i, ToValue j, ToValue k, ToValue l) => ToRow (a, b, c, d, e, f, g, h, i, j, k, l) where
  toRow (a, b, c, d, e, f, g, h, i, j, k, l) = toRow [en a, en b, en c, en d, en e, en f, en g, en h, en i, en j, en k, en l]

instance {-# OVERLAPPING #-} (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f, ToValue g, ToValue h, ToValue i, ToValue j, ToValue k, ToValue l, ToValue m) => ToRow (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  toRow (a, b, c, d, e, f, g, h, i, j, k, l, m) = toRow [en a, en b, en c, en d, en e, en f, en g, en h, en i, en j, en k, en l, en m]

encodeWith :: (Foldable t, ToRow a) => ParserConfig -> t a -> LazyByteString
encodeWith config rows = toLazyByteString $ runReader (foldApp toRow rows) config

encode :: (Foldable t, ToRow a) => t a -> LazyByteString
encode = encodeWith def
