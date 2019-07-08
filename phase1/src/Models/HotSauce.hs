module Models.HotSauce where

import Models.Entity
import Models.Pepper
import DB

import Data.List (sort)

type HotSauceId = Int

type Kg = Float

data Form = Fermented | Raw | Roasted
          deriving (Eq, Show, Read)

data HotSauce = HotSauce {
  hotSauceId :: HotSauceId,
  hName :: String,
  peppers :: [(PepperId, Kg, Form)],
  hRating :: Int
} deriving (Eq, Show, Read)

-- Constructor alias
hotSauce = HotSauce

instance Entity HotSauce where
    getId = hotSauceId

-- (3) Validations -----

validateHotSauce :: HotSauce -> Table Pepper -> Bool
validateHotSauce h t = validateRating (hRating h) && validateFKey h t

validateFKey :: HotSauce -> Table Pepper -> Bool
validateFKey h t = let hp = sort $ map fst' (peppers h)
                       tp = sort $ map pepperId t
                       in hp == tp
  where
    fst' (a,b,c) = a
