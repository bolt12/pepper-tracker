module Models.Pepper where

import Models.Entity
import Data.Time

-- (1) Type Definitions -----

type PepperId = Int

data Pepper = Pepper {
  pepperId :: PepperId,
  pName :: String,
  scoville :: Int,
  planted :: UTCTime,
  firstFruit :: Maybe UTCTime,
  pRating :: Int,
  active :: Bool
} deriving (Eq, Show, Read)

-- Constructor alias
pepper = Pepper

instance Entity Pepper where
    getId = pepperId

-- (3) Validations -----

validatePepper :: Pepper -> Bool
validatePepper p = validateRating (pRating p)

validateRating :: Int -> Bool
validateRating r = r > -1 && r < 11
