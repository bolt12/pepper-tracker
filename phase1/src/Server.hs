{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving
             , ScopedTypeVariables #-}

module WebServer.Server where

import DB
import Models

import Control.Monad.IO.Class
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Time

import Data.Text.Lazy (Text)
import Data.String

db1 = "PersistentFiles/peppers.db"
db2 = "PersistentFiles/hotSauces.db"

-- Global App State.
-- This state will be fetched from the DB and be kept in-memory.
data AppState = AppState { 
              getPeppers :: Table Pepper, 
              getPepperId :: Int, 
              getHotSauces :: Table HotSauce,
              getHotSauceId :: Int
              }

-- Default AppState.
-- Can be seen as the initial state.
def = do
    peppers <- load db1
    hotSauces <- load db2
    return $ AppState peppers 0 hotSauces 0

-- Wrapper type around the ReaderT that will keep the state.
-- TVar provides a shared memory locations that support atomic memory transactions.
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
                 deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
    def' <- def
    sync <- newTVarIO def'
    -- 'runActionToIO' is called once per action.
    let runActionToIO m = runReaderT (runWebM m) sync

    scottyT 3000 runActionToIO app

-- This app doesn't use raise/rescue, so the exception
-- type is ambiguous. We can fix it by putting a type
-- annotation just about anywhere. In this case, we'll
-- just do it on the entire app.
app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev

    get "/peppers" $ do
        ps <- webM $ gets getPeppers
        text $ fromString $ show ps

    get "/:addPepper" $ do
        id <- webM $ gets getPepperId
        name <- fromString <$> param "name"
        scoville <- param "scoville"
        rating <- param "rating"
        active <- param "active"
        time <- liftIO getCurrentTime
        let pp = pepper id name scoville time time rating active
        ps <- webM $ gets getPeppers

        webM $ modify $ \ st -> st { getPeppers = pp : ps, getPepperId = id + 1 }
        liftIO $ persist db1 (pp:ps)
        redirect "/peppers"
