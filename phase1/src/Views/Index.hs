{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import Lucid

index :: Html ()
index = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Pepper Tracker"
    h4_ "Choose page"
    div_ $ do
      button_ [onclick_ "window.location='http://localhost:3000/peppers'"] "Peppers"
      button_ [onclick_ "window.location='http://localhost:3000/hotsauces'"] "Hot Sauces"
