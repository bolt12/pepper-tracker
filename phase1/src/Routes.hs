{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( routes
  )
where

import           DB
import Models.Pepper
import Models.HotSauce
import           AppState
import Views.Index
import Views.Peppers
import Views.PepperAdd
import Views.PepperEdit
import Views.PepperDelete
import Views.ErrorPage

import           Control.Monad.Reader
import           Data.String
import           Data.Time
import Lucid

import           Web.Scotty.Trans

routes :: ScottyError e => ScottyT e WebM ()
routes = do
  get "/" showIndex

  get "/peppers" getPeppersRouteS
  get "/peppers" (errorPage' "No peppers currently...")

  get "/pepperAdd" getPepperAddS
  get "/pepperAdd" (errorPage' "Failed!")

  get "/pepperEdit/:id" getPepperEditS
  get "/pepperEdit/:id" (errorPage' "Pepper not found!")

  get "/pepperDelete/:id" getPepperDeleteS
  get "/pepperDelete/:id" (errorPage' "Pepper not found!")

  post "/pepper" addPepperRouteS
  post "/pepper" (errorPage' "Failed!")

  post "/pepperEdit/:id" updatePepperRouteS
  post "/pepperEdit/:id" (errorPage' "Pepper not found!")

  post "/pepperDelete/:id" deletePepperRouteS
  post "/pepperDelete/:id" (errorPage' "Pepper not found!")

showIndex :: (ScottyError e) => ActionT e WebM ()
showIndex = html . renderText $ index

-- Error page
errorPage' :: (ScottyError e) => String -> ActionT e WebM ()
errorPage' = html . renderText . errorPage

-- Get Peppers Success endpoint
getPeppersRouteS :: (ScottyError e) => ActionT e WebM ()
getPeppersRouteS = do
  ps <- webM $ gets getPeppers
  html . renderText $ showPeppers ps

getPepperAddS :: (ScottyError e) => ActionT e WebM ()
getPepperAddS = html . renderText $ showPepperAdd

getPepperEditS :: (ScottyError e) => ActionT e WebM ()
getPepperEditS = do
  id <- param "id" `rescue` const next
  ps <- webM $ gets getPeppers
  let ppOgM = getEntity id ps
  if ppOgM == Nothing 
     then next
     else 
     let (Just p) = ppOgM
      in html . renderText $ showPepperEdit p

getPepperDeleteS :: (ScottyError e) => ActionT e WebM ()
getPepperDeleteS = do
  id <- param "id" `rescue` const next
  ps <- webM $ gets getPeppers
  let ppOgM = getEntity id ps
  if ppOgM == Nothing 
     then next
     else 
     let (Just p) = ppOgM
      in html . renderText $ showPepperDelete p

addPepperRouteS :: (ScottyError e) => ActionT e WebM ()
addPepperRouteS = do
  ps <- webM $ gets getPeppers
  let id = if null ps then 0 else maximum (map pepperId ps) + 1
  name       <- fromString <$> param "name" `rescue` const next
  scoville   <- param "scoville" `rescue` const next
  planted    <- param "planted" `rescue` const next
  firstFruit <- param "firstFruit" `rescue` const (return "")
  active     <- param "active" `rescue` const next
  rating     <- param "rating" `rescue` const next
  planted'   <- parseTimeM True defaultTimeLocale format planted
    `rescue` const next
  let firstFruit' = parseTimeM True defaultTimeLocale format firstFruit
      pp = pepper id name scoville planted' firstFruit' rating active
  if not (validatePepper pp)
    then next
    else do
      ps <- webM $ gets getPeppers
      let ps' = insertEntity pp ps
       in saveAndPersistPeppers ps'
  where format = "%F" -- %Y-%m-%d

updatePepperRouteS :: (ScottyError e) => ActionT e WebM ()
updatePepperRouteS = do
  id         <- param "id" `rescue` const next
  name       <- fromString <$> param "name" `rescue` const (return "")
  scoville   <- param "scoville" `rescue` const (return (-1))
  planted    <- param "planted" `rescue` const (return "")
  firstFruit <- param "firstFruit" `rescue` const (return "")
  active     <- param "active" `rescue` const (return False)
  rating     <- param "rating" `rescue` const (return (-1))
  ps         <- webM $ gets getPeppers
  let ppOgM = getEntity id ps
  if ppOgM == Nothing
    then next
    else
      let
        firstFruit' = parseTimeM True defaultTimeLocale format firstFruit
        planted'    = parseTimeM True defaultTimeLocale format planted
        (Just ppOg) = ppOgM
        ppOg1       = if null name then ppOg else ppOg { name = name }
        ppOg2 =
          if scoville < 0 then ppOg1 else ppOg1 { scoville = scoville }
        ppOg4 = if firstFruit' == Nothing
          then ppOg3
          else ppOg3 { firstFruit = firstFruit' }
        ppOg3 = if planted' == Nothing
          then ppOg2
          else let (Just p) = planted' in ppOg2 { planted = p }
        ppOg5 = if not active then ppOg4 else ppOg4 { active = active }
        ppOg6 = if rating < 0 then ppOg5 else ppOg5 { pRating = rating }
        ps'   = updateEntity ppOg6 ps
      in
        saveAndPersistPeppers ps'
  where format = "%F"

deletePepperRouteS :: (ScottyError e) => ActionT e WebM ()
deletePepperRouteS = do
  id <- param "id" `rescue` const next
  ps <- webM $ gets getPeppers
  let (_, ps') = deleteEntity id ps
  saveAndPersistPeppers ps'

-- Auxiliar functions

saveAndPersistPeppers :: (ScottyError e) => Table Pepper -> ActionT e WebM ()
saveAndPersistPeppers ps = do
  webM $ modify $ \st -> st { getPeppers = ps }
  state <- webM getSt
  liftIO $ persist dbFile state
  redirect "/peppers"
