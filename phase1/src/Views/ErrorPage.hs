{-# LANGUAGE OverloadedStrings #-}

module Views.ErrorPage where

import Lucid

import Data.String

errorPage :: String -> Html ()
errorPage msg = doctypehtml_ $ do
  head_ $
    title_ "Pepper Tracker"
  body_ $
    h2_ (fromString msg)
