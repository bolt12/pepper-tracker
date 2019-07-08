{-# LANGUAGE OverloadedStrings #-}

module Views.PepperEdit where

import Lucid
import Models.Pepper
import Views.ErrorPage

import Data.String

showPepperEdit :: Pepper -> Html ()
showPepperEdit p = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Pepper Edit"
    div_ $
      pepper2Html p

pepper2Html :: Pepper -> Html ()
pepper2Html p = 
  if active p 
     then div_ $
       form_ [method_ "post", action_ (fromString $ "/pepperEdit/" ++ show (pepperId p))] $ do
         label_ $ do
           "Name: "
           input_ [name_ "name", placeholder_ (fromString $ pName p)]
         label_ $ do
           "Scoville: "
           input_ [name_ "scoville", value_ "-1", placeholder_ (fromString . show $ scoville p)]
         label_ $ do
           "Planted: "
           input_ [name_ "planted", placeholder_ (fromString . show $ planted p)]
         label_ $ do
           "First fruit: "
           input_ [name_ "first fruit", placeholder_ "YY-MM-DD"]
         label_ $ do
           "rating: "
           input_ [name_ "rating", value_ "-1", placeholder_ (fromString . show $ pRating p)]
         input_ [type_ "submit", value_ "Edit pepper"]
     else errorPage "Pepper not found!"
