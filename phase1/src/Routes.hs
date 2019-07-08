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
import Views.HotSauces
import Views.HotSauceAdd
import Views.HotSauceAddPepper
import Views.HotSauceEdit
import Views.HotSauceDelete
import Views.ErrorPage

import           Control.Monad.Reader
import           Data.String
import           Data.Time
import Lucid

import           Web.Scotty.Trans

routes :: ScottyError e => ScottyT e WebM ()
routes = do
  get "/" showIndex

-- Pepper Routes
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

-- HotSauce Routes
  get "/hotsauces" getHotSaucesRouteS
  get "/hotsauces" (errorPage' "No hot sauces currently...")

  get "/hotsauceAdd" getHotSauceAddS
  get "/hotsauceAdd" (errorPage' "Failed!")

  get "/hotsauceEdit/:id" getHotSauceEditS
  get "/hotsauceEdit/:id" (errorPage' "HotSauce not found!")

  get "/hotsauceAddPepper/:id" getHotSauceAddPepperS
  get "/hotsauceAddPepper/:id" (errorPage' "Failed!")

  get "/hotsauceDelete/:id" getHotSauceDeleteS
  get "/hotsauceDelete/:id" (errorPage' "HotSauce not found!")

  post "/hotsauce" addHotSauceRouteS
  post "/hotsauce" (errorPage' "Failed!")

  post "/hotsauceEdit/:id" updateHotSauceRouteS
  post "/hotsauceEdit/:id" (errorPage' "HotSauce not found!")

  post "/hotsauceAddPepper/:hsId/" addHotSaucePepperS
  post "/hotsauceAddPepper/:hsId/" (errorPage' "Failed!")

  post "/hotsauceDelete/:id" deleteHotSauceRouteS
  post "/hotsauceDelete/:id" (errorPage' "HotSauce not found!")

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
        ppOg1       = if null name then ppOg else ppOg { pName = name }
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

-- Get HotSauces Success endpoint
getHotSaucesRouteS :: (ScottyError e) => ActionT e WebM ()
getHotSaucesRouteS = do
  hs <- webM $ gets getHotSauces
  html . renderText $ showHotSauces hs

getHotSauceAddS :: (ScottyError e) => ActionT e WebM ()
getHotSauceAddS = html . renderText $ showHotSauceAdd

getHotSauceEditS :: (ScottyError e) => ActionT e WebM ()
getHotSauceEditS = do
  id <- param "id" `rescue` const next
  hs <- webM $ gets getHotSauces
  let hsOgM = getEntity id hs
  if hsOgM == Nothing 
     then next
     else 
     let (Just h) = hsOgM
      in html . renderText $ showHotSauceEdit h

getHotSauceAddPepperS :: (ScottyError e) => ActionT e WebM ()
getHotSauceAddPepperS = do
  id <- param "id" `rescue` const next
  hs <- webM $ gets getHotSauces
  ps <- webM $ gets getPeppers
  let hsOgM = getEntity id hs
  if hsOgM == Nothing 
     then next
     else 
     let (Just h) = hsOgM
      in html . renderText $ showHotSauceAddPepper h ps

getHotSauceDeleteS :: (ScottyError e) => ActionT e WebM ()
getHotSauceDeleteS = do
  id <- param "id" `rescue` const next
  hs <- webM $ gets getHotSauces
  let hsOgM = getEntity id hs
  if hsOgM == Nothing 
     then next
     else 
     let (Just h) = hsOgM
      in html . renderText $ showHotSauceDelete h

addHotSauceRouteS :: (ScottyError e) => ActionT e WebM ()
addHotSauceRouteS = do
  hs <- webM $ gets getHotSauces
  let id = if null hs then 0 else maximum (map hotSauceId hs) + 1
  name     <- param "name" `rescue` const next
  rating     <- param "rating" `rescue` const next
  let hotsauce = hotSauce id name [] rating
  if not (validateHotSauce hotsauce [])
    then next
    else do
      ps <- webM $ gets getHotSauces
      let hs' = insertEntity hotsauce hs
       in saveAndPersistHotSauces hs'

addHotSaucePepperS :: (ScottyError e) => ActionT e WebM ()
addHotSaucePepperS = do
  hsId     <- param "hsId" `rescue` const next
  pId     <- param "pId" `rescue` const next
  kg     <- param "kg" `rescue` const next
  form     <- read <$> param "form" `rescue` const next
  pps <- webM $ gets getPeppers
  hs         <- webM $ gets getHotSauces
  let p = getEntity pId pps
      h = getEntity hsId hs
  if p /= Nothing && h /= Nothing
     then
      let
        (Just hsOg) = h
        pps2 = (pId, kg, form) : peppers hsOg
        hsOg1 = hsOg { peppers = pps2 }
        hs'   = updateEntity hsOg1 hs
      in
        saveAndPersistHotSauces hs'
     else next

updateHotSauceRouteS :: (ScottyError e) => ActionT e WebM ()
updateHotSauceRouteS = do
  id         <- param "id" `rescue` const next
  name       <- fromString <$> param "name" `rescue` const (return "")
  rating     <- param "rating" `rescue` const (return (-1))
  hs         <- webM $ gets getHotSauces
  let hsOgM = getEntity id hs
  if hsOgM == Nothing
    then next
    else
      let
        (Just hsOg) = hsOgM
        hsOg1       = if null name then hsOg else hsOg { hName = name }
        hsOg2 = if rating < 0 then hsOg1 else hsOg1 { hRating = rating }
        hs'   = updateEntity hsOg2 hs
      in
        saveAndPersistHotSauces hs'

deleteHotSauceRouteS :: (ScottyError e) => ActionT e WebM ()
deleteHotSauceRouteS = do
  id <- param "id" `rescue` const next
  ps <- webM $ gets getHotSauces
  let (_, ps') = deleteEntity id ps
  saveAndPersistHotSauces ps'

-- Auxiliar functions

saveAndPersistPeppers :: (ScottyError e) => Table Pepper -> ActionT e WebM ()
saveAndPersistPeppers ps = do
  webM $ modify $ \st -> st { getPeppers = ps }
  state <- webM getSt
  liftIO $ persist dbFile state
  redirect "/peppers"

saveAndPersistHotSauces :: (ScottyError e) => Table HotSauce -> ActionT e WebM ()
saveAndPersistHotSauces hs = do
  webM $ modify $ \st -> st { getHotSauces = hs }
  state <- webM getSt
  liftIO $ persist dbFile state
  redirect "/hotsauces"
