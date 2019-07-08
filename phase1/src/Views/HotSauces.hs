{-# LANGUAGE OverloadedStrings #-}

module Views.HotSauces where

import Lucid
import Models.HotSauce
import Models.Pepper
import DB

import Data.String

showHotSauces :: Table HotSauce -> Table Pepper -> Html ()
showHotSauces t t' = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $ do 
    h2_ "Hot Sauces"
    div_ $
      mapM_ (hotSauce2Html t') t
    div_ $
      button_ [onclick_ (fromString $ "window.location=" ++ "'" ++ add ++ "'")] "Add hot sauce"
  where
    add = "http://localhost:3000/hotsauceAdd/" 

hotSauce2Html :: Table Pepper -> HotSauce -> Html ()
hotSauce2Html t h = 
     div_ $ do 
       ul_ $ do
         li_ . toHtml $ ("Name: " ++ hName h)
         li_ $ do
           "Peppers: "
           ul_ $
             mapM_ (pp2Html t) (peppers h)
         li_ . toHtml $ ("Rating: " ++ show (hRating h))
       button_ [onclick_ (fromString $ "window.location.href=" ++ "'" ++ edit ++ "'")] "Edit"
       button_ [onclick_ (fromString $ "window.location.href="++ "'" ++ delete ++ "'")] "Delete"
  where
    edit = "http://localhost:3000/hotsauceEdit/" ++ show (hotSauceId h)
    delete = "http://localhost:3000/hotsauceDelete/" ++ show (hotSauceId h)
    pp2Html :: Table Pepper -> (PepperId, Kg, Form) -> Html ()
    pp2Html t (id, kg, form) = do 
      let (Just p') = getEntity id t
      li_ $ do
        div_ . toHtml $ ("Name: " ++ pName p')
        div_ . toHtml $ ("Kg: " ++ show kg)
        div_ . toHtml $ ("Form: " ++ show form)
