{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Views.HotSauceAddPepper where

import Lucid
import Models.HotSauce
import Models.Pepper

import Data.String

showHotSauceAddPepper :: HotSauce -> [Pepper] -> Html ()
showHotSauceAddPepper p t = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "HotSauce Add Pepper"
    div_ $
      hotSauce2Html p t

hotSauce2Html :: HotSauce -> [Pepper] -> Html ()
hotSauce2Html p t = 
     div_ $ 
       form_ [method_ "post", action_ (fromString $ "/hotsauceAddPepper/" ++ show (hotSauceId p))] $ do
         label_ "Select pepper: "
         select_ [name_ "pId"] $
             mapM_ optPepper t
         input_ [type_ "submit", value_ "Add pepper"]
  where
    optPepper p = option_ [value_ (ppId p)] "test" :: Html ()
    ppId p = fromString $ show $ pepperId p
