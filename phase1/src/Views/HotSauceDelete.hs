{-# LANGUAGE OverloadedStrings #-}

module Views.HotSauceDelete where

import Lucid
import Models.HotSauce

import Data.String

showHotSauceDelete :: HotSauce -> Html ()
showHotSauceDelete p = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Are you sure you want to delete?"
    div_ $
      hotSauce2Html p

hotSauce2Html :: HotSauce -> Html ()
hotSauce2Html h = 
     div_ $ do
       ul_ $ do
         li_ . toHtml $ ("Name: " ++ hName h)
         li_ . toHtml $ ("Rating: " ++ show (hRating h))
       form_ [method_ "post", action_ (fromString $ "/hotsauceDelete/" ++ show (hotSauceId h))] $
         input_ [type_ "submit", value_ "Delete"]
       button_ [onclick_ "window.location='http://localhost:3000/hotsauces'"] "Back"


