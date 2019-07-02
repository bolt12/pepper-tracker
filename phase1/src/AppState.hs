{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving
             , ScopedTypeVariables #-}

module AppState where 

import DB
import Models

import Control.Concurrent.STM
import Control.Monad.Reader

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

