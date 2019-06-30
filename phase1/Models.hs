{-# LANGUAGE DuplicateRecordFields #-}

module Models () where

import Data.Time

-- (1) Data types - Schema -----

data Pepper = Pepper {
  pepperId :: PepperId,
  name :: String,
  scoville :: Int,
  planted :: UTCTime,
  firstFruit :: UTCTime,
  rating :: Int,
  active :: Bool
}

type PepperId = Int
type HotSauceId= Int

type Kg = Float
type Month = Int

data Form = Fermented Month | Raw | Roasted

data HotSauce = HotSauce {
  hotSauceId :: HotSauceId,
  peppers :: [(PepperId, Kg, Form)],
  rating :: Int
}

-- (2) Validations -----

validateRating :: Int -> Bool
validateRating r = r > 0 && r < 11
