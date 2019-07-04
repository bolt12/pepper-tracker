module Models.HotSauce where

import Models.Entity
import Models.Pepper

type HotSauceId = Int

type Kg = Float
type Month = Int

data Form = Fermented Month | Raw | Roasted
          deriving (Eq, Show, Read)

data HotSauce = HotSauce {
  hotSauceId :: HotSauceId,
  peppers :: [(PepperId, Kg, Form)],
  rating :: Int
} deriving (Eq, Show, Read)

-- Constructor alias
hotSauce = HotSauce

instance Entity HotSauce where
    getId = hotSauceId

