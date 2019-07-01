module Models.HotSauce where

import Models.Entity
import Models.Utils
import Models.Pepper
import Text.Megaparsec
import Text.Megaparsec.Char

type HotSauceId = Int

type Kg = Float
type Month = Int

data Form = Fermented Month | Raw | Roasted
          deriving (Show, Read)

data HotSauce = HotSauce {
  hotSauceId :: HotSauceId,
  peppers :: [(PepperId, Kg, Form)],
  rating :: Int
} deriving (Show, Read)

hotSauce = HotSauce

instance Entity HotSauce where
    getId = hotSauceId

