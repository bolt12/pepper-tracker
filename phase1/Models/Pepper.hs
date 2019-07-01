module Models.Pepper where

import Models.Entity
import Models.Utils
import Data.Time
import Text.Megaparsec
import Text.Megaparsec.Char

-- (1) Type Definitions -----

type PepperId = Int

data Pepper = Pepper {
  pepperId :: PepperId,
  name :: String,
  scoville :: Int,
  planted :: UTCTime,
  firstFruit :: UTCTime,
  rating :: Int,
  active :: Bool
} deriving (Show, Read)

pepper = Pepper

instance Entity Pepper where
    getId = pepperId

-- (3) Validations -----

validateRating :: Int -> Bool
validateRating r = r > 0 && r < 11
