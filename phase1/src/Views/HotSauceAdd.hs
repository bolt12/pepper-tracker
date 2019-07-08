{-# LANGUAGE OverloadedStrings #-}

module Views.HotSauceAdd where

import Lucid

showHotSauceAdd :: Html ()
showHotSauceAdd = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Hot Sauce Add"
    div_ hotSauceForm

hotSauceForm :: Html ()
hotSauceForm = 
     div_ $
       form_ [method_ "post", action_ "/hotsauce"] $ do
         label_ $ do
           "Name: "
           input_ [name_ "name", placeholder_ "Ultra hot sauce"]
         label_ $ do
           "Rating: "
           input_ [name_ "rating", value_ "0"]
         input_ [type_ "submit", value_ "Add hot sauce"]

