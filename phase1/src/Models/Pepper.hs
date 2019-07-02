module Models.Pepper where

import Models.Entity
import Data.Time

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

-- Constructor alias
pepper = Pepper

instance Entity Pepper where
    getId = pepperId

-- (3) Validations -----

validateRating :: Int -> Bool
validateRating r = r > 0 && r < 11
