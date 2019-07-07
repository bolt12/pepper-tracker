{-# LANGUAGE OverloadedStrings #-}

module Views.Peppers where

import Lucid
import Models.Pepper
import DB

import Data.String

showPeppers :: Table Pepper -> Html ()
showPeppers t = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Peppers"
    div_ $
      mapM_ pepper2Html t
    div_ $
      button_ [onclick_ (fromString $ "window.location=" ++ "'" ++ add ++ "'")] "Add pepper"
  where
    add = "http://localhost:3000/pepperAdd/" 

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
       button_ [onclick_ (fromString $ "window.location.href=" ++ "'" ++ edit ++ "'")] "Edit"
       button_ [onclick_ (fromString $ "window.location.href="++ "'" ++ delete ++ "'")] "Delete"
     else ""
  where
    edit = "http://localhost:3000/pepperEdit/" ++ show (pepperId p)
    delete = "http://localhost:3000/pepperDelete/" ++ show (pepperId p)
