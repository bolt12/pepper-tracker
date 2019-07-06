{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving
             , ScopedTypeVariables #-}

module AppState where

import           Models
import           DB

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           System.Directory

dbFile = "src/PersistentFiles/db.db"

-- Global App State.
-- This state will be fetched from the DB and be kept in-memory.
data AppState = AppState {
              getPeppers :: Table Pepper,
              getHotSauces :: Table HotSauce
              }
              deriving (Show, Eq, Read)

-- Default AppState.
-- Can be seen as the initial state.
def :: AppState
def = AppState [] []

-- Wrapper type around the ReaderT that will keep the state.
-- TVar provides a shared memory locations that support atomic memory transactions.
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
                 deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
getSt :: WebM AppState
getSt = ask >>= liftIO . readTVarIO

gets :: (AppState -> b) -> WebM b
gets f = f <$> getSt

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

-- Serialize DB into a file
persist :: FilePath -> AppState -> IO ()
persist file = writeFile file . show

-- Deserialize DB from a file
load :: FilePath -> IO AppState
load file = do
  r <- doesFileExist file
  if r
    then do
      txt <- readFile file
      return . read $ txt
    else return def
