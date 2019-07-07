{-# LANGUAGE OverloadedStrings #-}

module Views.PepperAdd where

import Lucid

showPepperAdd :: Html ()
showPepperAdd = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Pepper Edit"
    div_ pepperForm

pepperForm :: Html ()
pepperForm = 
     div_ $
       form_ [method_ "post", action_ "/pepper"] $ do
         label_ $ do
           "Name: "
           input_ [name_ "name", placeholder_ "Jalapeño"]
         label_ $ do
           "Scoville: "
           input_ [name_ "scoville", value_ "0"]
         label_ $ do
           "Planted: "
           input_ [name_ "planted", placeholder_ "YY-MM-DD"]
         label_ $ do
           "First fruit: "
           input_ [name_ "firstFruit", placeholder_ "YY-MM-DD"]
         label_ $ do
           "Rating: "
           input_ [name_ "rating", value_ "0"]
         label_ $ do
           "Active: "
           input_ [name_ "active", type_ "radio", value_ "True"]
         label_ $ do
           "Inactive: "
           input_ [name_ "active", type_ "radio", value_ "False"]
         input_ [type_ "submit", value_ "Add pepper"]
