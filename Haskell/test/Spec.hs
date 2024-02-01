{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import Data.ByteString.Lazy (LazyByteString)
import Data.Default
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.RSV
import Data.Scientific
import Data.Text (Text)
import Data.UUID (UUID)
import Test.Hspec
import Text.Printf

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.UUID as UUID

toHexList :: LazyByteString -> String
toHexList lbs = "[" <> toHexList' lbs <> "]"
  where
    toHexList' = intercalate ", " . map (printf "0x%02X") . LB.unpack

data Person = Person {
  id :: !UUID,
  name :: !Text,
  birthYear :: !(Maybe Int)
} deriving (Eq, Show)

instance ToRow Person where
  toRow Person{..} = encodeRow $ toValue id <> toValue name <> toValue birthYear

instance FromRow Person where
  fromRow = parseRow $ Person <$> fromValue <*> fromValue <*> fromValue

data X = XInt !Int | XText !Text deriving (Eq, Show)

instance ToRow X where
  toRow (XInt i) = encodeRow $ toValue i
  toRow (XText t) = encodeRow $ toValue t

instance FromRow X where
  fromRow = parseRow $ (XInt <$> fromValue) <|> (XText <$> fromValue)

roundtripWith :: (Foldable t, ToRow a, FromRow a) => ParserConfig -> t a -> ParseResult [a] 
roundtripWith config = parseWith config . encodeWith config 

roundtrip :: (Foldable t, ToRow a, FromRow a) => t a -> ParseResult [a] 
roundtrip = roundtripWith def

main :: IO ()
main = hspec $ do
  describe "RSV" $ do
    it "encodes to UTF-8 except for special delimiters." $ do
      let input = [(Nothing :: Maybe Integer, "i" :: Text, 1 :: Int, 2.7e3 :: Scientific, True)]
      let lbs = encode input
      lbs `shouldBe` LB.pack [0xFE, 0xFF, 0x69, 0xFF, 0x31, 0xFF, 0x32, 0x37, 0x30, 0x30, 0x2E, 0x30, 0xFF, 0x74, 0x72, 0x75, 0x65, 0xFF, 0xFD]
    it "decodes from UTF-8 except for special delimiters." $ do
      let lbs = LB.pack [0xFE, 0xFF, 0x69, 0xFF, 0x31, 0xFF, 0x32, 0x37, 0x30, 0x30, 0x2E, 0x30, 0xFF, 0x74, 0x72, 0x75, 0x65, 0xFF, 0xFD]
      let output = [(Nothing :: Maybe Integer, "i" :: Text, 1 :: Int, 2.7e3 :: Scientific, True)]
      case parse lbs of
        Left e -> expectationFailure (show e)
        Right output' -> output' `shouldBe` output
    it "wants to parse fully before unexpected EOF" $ do
      let lbs = LB.pack [0xFE]
      case parse lbs :: ParseResult [[Maybe Int]] of
        Left (indices, Unexpected EOF) -> indices `shouldBe` (1, 0, 0) 
        Left e -> expectationFailure (show e)
        _ -> expectationFailure "Expected to fail with UnexpectedEOF at the correct indices"
    it "wants row terminators to be in the proper location" $ do
      let lbs = LB.pack [0xFE, 0xFD, 0xFF]
      case parse lbs :: ParseResult [[Maybe Text]] of
        Left (indices, Expected ValueMarker) -> indices `shouldBe` (1, 0, 0) 
        Left e -> expectationFailure (show e)
        _ -> expectationFailure "Expected to fail with UnexpectedRowTerminator at the correct indices"
    it "wants nulls to be solo with no preceding UTF-8 characters" $ do
      let lbs = LB.pack [0x69, 0xFE, 0xFF, 0xFD]
      case parse lbs :: ParseResult [[Maybe String]] of
        Left (indices, Unexpected NullMarker) -> indices `shouldBe` (1, 0, 0) 
        Left e -> expectationFailure (show e)
        _ -> expectationFailure "Expected to fail with UnexpectedNull at the correct indices"
    it "wants nulls to be solo with no following UTF-8 characters" $ do
      let lbs = LB.pack [0xFE, 0x69, 0xFF, 0xFD]
      case parse lbs :: ParseResult [[Maybe String]] of
        Left (indices, Expected ValueMarker) -> indices `shouldBe` (1, 0, 0) 
        Left e -> expectationFailure (show e)
        _ -> expectationFailure "Expected to fail with UnexpectedNull at the correct indices"
    it "wants to parse valid UTF-8" $ do
      let lbs = LB.pack [0xFE, 0xFF, 0xC2, 0x20, 0x20, 0xFF, 0xFD]
      case parse lbs :: ParseResult [[Maybe Text]] of 
        Left (indices, UnicodeError) -> indices `shouldBe` (6, 2, 0)
        Left e -> expectationFailure (show e)
        _ -> expectationFailure "Expected to fail with a UnicodeError"
    it "supports arbitrary Haskell types" $ do
      let uuid = fromJust $ UUID.fromString "FDBF6987-979D-4B16-A126-4C6171368B45"
      let people = [Person uuid "Morrissey" (Just 1959)]
      case roundtrip people of
        Left e -> expectationFailure (show e)
        Right people' -> people' `shouldBe` people
    it "throws a ConversionError if a value can't be converted to Bool" $ do
      let lbs = LB.pack [0x7A, 0xFF, 0xFD]
      case parse lbs :: ParseResult [[Bool]] of
        Left (indices, ConversionError "Could not convert string z to desired type Bool.") -> indices `shouldBe` (2, 0, 0) 
        Left e -> expectationFailure (show e)
        _ -> expectationFailure "Expected to throw a ConversionError but it succeeded."
    it "supports Alternative when parsing" $ do
      let input = [XInt 3, XText "three"]
      case roundtrip input of
        Left e -> expectationFailure (show e)
        Right input' -> input' `shouldBe` input
    it "supports a 2-tuple as a row and encodes a strict ByteString in Base-64" $ do
      let tuple = [(7.5 :: Double, SB.pack [0xC2, 0x20, 0x20])]
      let lbs = encode tuple
      -- Note that C22020 becomes 77694167, i.e., wiAg, because it is Base-64 encoded.
      lbs `shouldBe` LB.pack [0x37, 0x2E, 0x35, 0xFF, 0x77, 0x69, 0x41, 0x67, 0xFF, 0xFD] 
      case parse lbs of
        Left e -> expectationFailure (show e)
        Right tuple' -> tuple' `shouldBe` tuple
    it "supports a 3-tuple as a row and encodes a lazy ByteString in Base-64" $ do
      let tuple = [(7 :: Integer, LB.pack [0xC2, 0x20, 0x20], Just "x" :: Maybe String)]
      let lbs = encode tuple
      -- Note that C22020 becomes 77694167, i.e., wiAg, because it is Base-64 encoded.
      lbs `shouldBe` LB.pack [0x37, 0xFF, 0x77, 0x69, 0x41, 0x67, 0xFF, 0x78, 0xFF, 0xFD] 
      case parse lbs of
        Left e -> expectationFailure (show e)
        Right tuple' -> tuple' `shouldBe` tuple
    it "supports a 4-tuple as a row and allows arbitrary values for True and False" $ do
      let config :: ParserConfig = def {falseValue = "z"}
      let tuple = [(False, False, False, False)]
      let lbs = encodeWith config tuple
      lbs `shouldBe` LB.pack [0x7A, 0xFF, 0x7A, 0xFF, 0x7A, 0xFF, 0x7A, 0xFF, 0xFD]    
      case parseWith config lbs of
        Left e -> expectationFailure (show e)
        Right tuple' -> tuple' `shouldBe` tuple
