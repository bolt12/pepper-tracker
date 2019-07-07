{-# LANGUAGE OverloadedStrings #-}

module Views.PepperDelete where

import Lucid
import Models.Pepper
import Views.ErrorPage

import Data.String

showPepperDelete :: Pepper -> Html ()
showPepperDelete p = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Are you sure you want to delete?"
    div_ $
      pepper2Html p

pepper2Html :: Pepper -> Html ()
pepper2Html p = 
  if active p 
     then div_ $ do
       ul_ $ do
         li_ . toHtml $ ("Name: " ++ name p)
         li_ . toHtml $ ("Scoville: " ++ show (scoville p))
         li_ . toHtml $ ("Planted: " ++ show (planted p))
         li_ . toHtml $ ("First fruit: " ++ show (planted p))
         li_ . toHtml $ ("Rating: " ++ show (pRating p))
       form_ [method_ "post", action_ (fromString $ "/pepperDelete/" ++ show (pepperId p))] $
         input_ [type_ "submit", value_ "Delete"]
       button_ [onclick_ "window.location='http://localhost:3000/peppers'"] "Back"
     else errorPage "Pepper not found!"

