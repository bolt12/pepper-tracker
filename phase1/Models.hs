{-# LANGUAGE DuplicateRecordFields #-}

module Models () where

import Data.Time
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

-- (1) Data types - Schema -----

data Pepper = Pepper {
  pepperId :: PepperId,
  name :: String,
  scoville :: Int,
  planted :: UTCTime,
  firstFruit :: UTCTime,
  rating :: Int,
  active :: Bool
} deriving (Show)

type PepperId = Int
type HotSauceId= Int

type Kg = Float
type Month = Int

data Form = Fermented Month | Raw | Roasted
          deriving Show

data HotSauce = HotSauce {
  hotSauceId :: HotSauceId,
  peppers :: [(PepperId, Kg, Form)],
  rating :: Int
} deriving Show

-- (2) Read Instance  + Parser -----

-- (2.1) Pepper Instance -----

instance Read Pepper where
    readsPrec _ = either (const []) (singl . split id (const "")) . parse parsePepper ""
      where
        singl a = [a]
        split f g a = (f a, g a)

type Parser = Parsec Void String

parsePepper :: Parser Pepper
parsePepper = do
    parseInit "Pepper"
    id <- parseInt "pepperId"
    parseSep ','
    name <- parseString "name"
    parseSep ','
    scoville <- parseInt "scoville"
    parseSep ','
    planted <- parseUTCTime "planted"
    parseSep ','
    firstFruit <- parseUTCTime "firstFruit"
    parseSep ','
    rating <- parseInt "rating"
    parseSep ','
    active <- parseBool "active"
    char '}'
    return $ Pepper id name scoville planted firstFruit rating active

parseInit :: String -> Parser ()
parseInit s = do
    string s
    skipSome (char '{' <|> char ' ')

parseSep :: Char -> Parser ()
parseSep c = skipMany (char c <|> char ' ')

parseInt :: String -> Parser Int
parseInt s = do
  string s
  parseSep '='
  read <$> some digitChar

parseString :: String -> Parser String
parseString s = do
  string s
  parseSep '='
  between (char '"') (char '"') (some alphaNumChar)

parseBool :: String -> Parser Bool
parseBool s = do
    string s
    parseSep '='
    read <$> (string "True" <|> string "False")

parseUTCTime :: String -> Parser UTCTime
parseUTCTime s = do
    string s
    parseSep '='
    read <$> someTill asciiChar (char ',')

-- (2.2) HotSauce Instance -----

-- (3) Validations -----

validateRating :: Int -> Bool
validateRating r = r > 0 && r < 11

-- (4) Dummy data -----

pepper1 :: IO Pepper
pepper1 = do
    time <- getCurrentTime
    return Pepper {
      pepperId = 0,
      name = "Serrano",
      scoville = 8000,
      planted = time,
      firstFruit = time,
      rating = 7,
      active = True
    }

pepper2 :: IO Pepper
pepper2 = do
    time <- getCurrentTime
    return Pepper {
      pepperId = 1,
      name = "Fataalli Yellow",
      scoville = 30000,
      planted = time,
      firstFruit = time,
      rating = 10,
      active = True
    }
