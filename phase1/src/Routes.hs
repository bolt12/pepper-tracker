{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( routes
  )
where

import           DB
import           Models
import           AppState

import           Control.Monad.Reader
import           Data.String
import           Data.Time

import           Web.Scotty.Trans

routes :: ScottyError e => ScottyT e WebM ()
routes = do
  get "/peppers"   getPeppersRouteS
  get "/peppers"   getPeppersRouteE

  get "/addPepper" addPepperRouteS
  get "/addPepper" addPepperRouteE

-- Get Peppers Success endpoint
getPeppersRouteS :: (ScottyError e) => ActionT e WebM ()
getPeppersRouteS = do
  ps <- webM $ gets getPeppers
  if null ps then next else html $ fromString $ show ps

-- Get Peppers Error endpoint
getPeppersRouteE :: (ScottyError e) => ActionT e WebM ()
getPeppersRouteE = html $ fromString "Currently there are no peppers to show"

addPepperRouteS :: (ScottyError e) => ActionT e WebM ()
addPepperRouteS = do
  id         <- webM $ gets getPepperId
  name       <- fromString <$> param "name" `rescue` const next
  scoville   <- param "scoville" `rescue` const next
  planted    <- param "planted" `rescue` const next
  firstFruit <- param "firstFruit" `rescue` const (return " ")
  active     <- param "active" `rescue` const next
  rating     <- param "rating" `rescue` const next
  planted'   <- parseTimeM True defaultTimeLocale format planted
    `rescue` const next
  let firstFruit' = parseTimeM True defaultTimeLocale format firstFruit
      pp          = pepper id name scoville planted' firstFruit' rating active
  if not (validatePepper pp)
    then next
    else do
      ps <- webM $ gets getPeppers

      webM $ modify $ \st -> st { getPeppers = pp : ps, getPepperId = id + 1 }
      liftIO $ persist db1 (pp : ps)
      redirect "/peppers"
  where format = "%F" -- %Y-%m-%d

addPepperRouteE :: (ScottyError e) => ActionT e WebM ()
addPepperRouteE = html $ fromString "Failed!"
