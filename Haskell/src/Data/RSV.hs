{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralisedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, RecordWildCards #-}

module Data.RSV (
  allTrueValues,
  allFalseValues,
  convertValue,
  conversionError,
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
  nullChar,
  parse,
  parseBinary,
  parseBinaryLazy,
  parseBool,
  parseRead,
  parseRow,
  parseString,
  parseText,
  parseWith,
  permitNull,
  rowTerminatorChar,
  runEncoder,
  throwIndexedException,
  toMarker,
  valueTerminatorChar,
  Encodable(..),
  Encoder,
  EncoderContext(..),
  FromRow(..),
  FromValue(..),
  IndexedException,
  Marker(..),
  Parser,
  ParserException(..),
  ParserConfig(..),
  ParserIndices,
  ParseResult,
  ParserState,
  ToRow(..),
  ToValue(..)
) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Bifunctor
import Data.ByteString (StrictByteString)
import Data.ByteString.Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.Default
import Data.Word
import Data.Scientific
import Data.Set (member, Set)
import Data.Text (Text)
import Data.UUID (UUID)
import Text.Printf
import Text.Read hiding (get, EOF)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID

valueTerminatorChar, nullChar, rowTerminatorChar :: Word8
valueTerminatorChar = 0xFF
nullChar = 0xFE
rowTerminatorChar = 0xFD

type ParserIndices = (Integer, Integer, Integer)
type ParserState = (ParserIndices, [Word8])

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

data Marker = ValueMarker | RowMarker | NullMarker | EOF deriving (Eq, Show)

toMarker :: Word8 -> Maybe Marker
toMarker 0xFD = Just RowMarker
toMarker 0xFE = Just NullMarker
toMarker 0xFF = Just ValueMarker
toMarker _ = Nothing 

data ParserException = Unexpected Marker | Expected Marker | UnicodeError | UnpermittedNull | ConversionError String deriving (Eq, Show)
type IndexedException = (ParserIndices, ParserException)

conversionError :: String
conversionError = "Could not convert string %s to desired type %s."

newtype Parser a = Parser { unParser :: RWST ParserConfig () ParserState (Either IndexedException) a }
  deriving (Functor, Applicative, Monad, MonadReader ParserConfig, MonadState ParserState, MonadError IndexedException)

throwIndexedException :: ParserException -> Parser a 
throwIndexedException e = do
  indices <- gets fst 
  throwError (indices, e)

liftEitherIndexed :: Either ParserException a -> Parser a
liftEitherIndexed e' = case e' of 
  Left e -> throwIndexedException e 
  Right a -> return a

instance Alternative Parser where
  empty = throwIndexedException $ Expected EOF
  a <|> b = catchError a (const b) 

instance Semigroup a => Semigroup (Parser a) where
  a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (Parser a) where
  mempty = Parser $ return mempty

consume :: Int -> Parser ()
consume n = do
  (indices@(i, v, r), bytes) <- get 
  if null bytes
    then throwError (indices, Unexpected EOF)
    else put ((i + fromIntegral n, v, r), drop n bytes)

advance :: Parser ()
advance = consume 1

next :: Parser Word8
next = do
  bytes <- gets snd
  case bytes of
    [] -> throwIndexedException $ Unexpected EOF
    (byte:_) -> return byte

value :: Parser [Word8]
value = parseValue $ null' <|> value' 
  where
    null' = do
      n <- next
      case toMarker n of
        Just NullMarker -> advance >> return [n]
        _ -> throwIndexedException $ Expected NullMarker
    value' = do 
      n <- next
      case toMarker n of
        Nothing -> advance >> (n :) <$> value'
        Just ValueMarker -> return []
        Just marker -> throwIndexedException $ Unexpected marker
    parseValue parser = do
      resetValueIndex
      a <- parser
      n <- next
      case toMarker n of
        Just ValueMarker -> do
          advance
          case a of
            [0xFE] -> throwIndexedException UnpermittedNull
            _ -> return a
        _ -> throwIndexedException $ Expected ValueMarker
    resetValueIndex :: Parser ()
    resetValueIndex = do
      (i, _, r) <- gets fst
      modify $ first $ const (i, i, r) 

convert :: (v -> Either ParserException a) -> v -> Parser a
convert f = liftEitherIndexed . f 

convertValue :: ([Word8] -> Either ParserException a) -> Parser a
convertValue f = value >>= convert f

parseText :: Parser Text
parseText = convertValue $ first (const UnicodeError) . TE.decodeUtf8' . SB.pack 

parseString :: Parser String
parseString = Text.unpack <$> parseText

parseBinary :: Parser StrictByteString
parseBinary = convertValue $ first ConversionError . B64.decode . SB.pack

parseBinaryLazy :: Parser LazyByteString
parseBinaryLazy = convertValue $ first ConversionError . B64L.decode . LB.pack

parseBool :: Parser Bool
parseBool = do 
  config <- ask
  t <- parseText
  case (t `member` allTrueValues config, t `member` allFalseValues config) of
    (True, _) -> return True
    (_, True) -> return False
    _ -> throwIndexedException $ ConversionError $ printf conversionError (Text.unpack t) ("Bool" :: String)

parseRead :: Read a => String -> Parser a
parseRead typeName = parseString >>= convert convertString 
  where
    convertString s = maybeToEither $ readMaybe s 
      where
        maybeToEither Nothing = Left $ ConversionError $ printf conversionError s typeName 
        maybeToEither (Just a) = Right a

permitNull :: Parser a -> Parser (Maybe a)
permitNull parser = catchError (Just <$> parser) handleNull
  where
    handleNull (_, UnpermittedNull) = consume 2 >> return Nothing 
    handleNull e = throwError e

class FromValue a where
  fromValue :: Parser a

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
  fromValue = parseString >>= convert toUUID
    where
      toUUID :: String -> Either ParserException UUID
      toUUID s = case UUID.fromString s of
        Just uuid -> return uuid
        Nothing -> throwError $ ConversionError $ printf "Could not convert %s to a UUID." s

instance FromValue StrictByteString where
  fromValue = parseBinary

instance FromValue LazyByteString where
  fromValue = parseBinaryLazy

instance FromValue a => FromValue (Maybe a) where
  fromValue = permitNull fromValue

parseList :: FromValue a => Parser [a]
parseList = do
  n <- next
  case toMarker n of
    Just RowMarker -> return []
    _ -> liftA2 (:) fromValue parseList 

parseRow :: Parser a -> Parser a
parseRow parser = do 
  resetRowIndex
  a <- parser
  n <- next
  case toMarker n of
    Just RowMarker -> advance >> return a 
    _ -> throwIndexedException $ Expected RowMarker 
  where
    resetRowIndex = do
      (i, _, _) <- gets fst
      modify $ first $ const (i, i, i)

class FromRow a where
  fromRow :: Parser a

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
parseWith config lbs = fst <$> evalRWST (unParser parse') config ((0, 0, 0), LB.unpack lbs) 
  where
    parse' = do
      bytes <- gets snd
      if null bytes
        then return []
        else liftA2 (:) fromRow parse'

parse :: FromRow a => LazyByteString -> ParseResult [a] 
parse = parseWith def

newtype EncoderContext a = EncoderContext (Reader ParserConfig a)
    deriving (Functor, Applicative, Monad, MonadReader ParserConfig)

runEncoder :: EncoderContext a -> ParserConfig -> a
runEncoder (EncoderContext reader) = runReader reader

instance Semigroup m => Semigroup (EncoderContext m) where
  a <> b = liftA2 (<>) a b

instance Monoid m => Monoid (EncoderContext m) where
  mempty = EncoderContext $ return mempty 

type Encoder = EncoderContext Builder 

encodeValue :: Encoder -> Encoder 
encodeValue encoder = encoder <> pure (word8 valueTerminatorChar)

encodeBuilder :: Builder -> Encoder
encodeBuilder = encodeValue . pure

encodeNull :: Encoder
encodeNull = encodeBuilder $ word8 nullChar 

encodeStringUnsafe :: String -> Encoder
encodeStringUnsafe = encodeBuilder . stringUtf8 

encodeText :: Text -> Encoder
encodeText = encodeStringUnsafe . Text.unpack

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
encodeRow = (<> pure (word8 rowTerminatorChar))

class ToRow a where
  toRow :: a -> Encoder

instance (Foldable t, ToValue a) => ToRow (t a) where
  toRow = encodeRow . foldMap toValue

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
encodeWith config rows = toLazyByteString $ runEncoder (foldMap toRow rows) config

encode :: (Foldable t, ToRow a) => t a -> LazyByteString
encode = encodeWith def
