{-# LANGUAGE OverloadedStrings #-}

module Views.HotSauceEdit where

import Lucid
import Models.HotSauce

import Data.String

showHotSauceEdit :: HotSauce -> Html ()
showHotSauceEdit p = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "HotSauce Edit"
    div_ $
      hotSauce2Html p

hotSauce2Html :: HotSauce -> Html ()
hotSauce2Html p = 
     div_ $ do
       form_ [method_ "post", action_ (fromString $ "/hotsauceEdit/" ++ show (hotSauceId p))] $ do
         label_ $ do
           "Name: "
           input_ [name_ "name", placeholder_ (fromString $ hName p)]
         label_ $ do
           "rating: "
           input_ [name_ "rating", value_ "-1"]
         input_ [type_ "submit", value_ "Edit Hot Sauce"]
       button_ [onclick_ (fromString $ "window.location='http://localhost:3000/hotsauceAddPepper/"++ show (hotSauceId p) ++ "'")] "Add pepper"

