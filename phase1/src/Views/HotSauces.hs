{-# LANGUAGE OverloadedStrings #-}

module Views.HotSauces where

import Lucid
import Models.HotSauce
import DB

import Data.String

showHotSauces :: Table HotSauce -> Html ()
showHotSauces t = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Hot Sauces"
    div_ $
      mapM_ hotSauce2Html t
    div_ $
      button_ [onclick_ (fromString $ "window.location=" ++ "'" ++ add ++ "'")] "Add hot sauce"
  where
    add = "http://localhost:3000/hotsauceAdd/" 

hotSauce2Html :: HotSauce -> Html ()
hotSauce2Html h = 
     div_ $ do 
       ul_ $ do
         li_ . toHtml $ ("Name: " ++ hName h)
         li_ . toHtml $ ("Rating: " ++ show (hRating h))
       button_ [onclick_ (fromString $ "window.location.href=" ++ "'" ++ edit ++ "'")] "Edit"
       button_ [onclick_ (fromString $ "window.location.href="++ "'" ++ delete ++ "'")] "Delete"
  where
    edit = "http://localhost:3000/hotsauceEdit/" ++ show (hotSauceId h)
    delete = "http://localhost:3000/hotsauceDelete/" ++ show (hotSauceId h)
