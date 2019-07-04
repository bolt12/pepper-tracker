{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           AppState
import           Routes

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Web.Scotty.Trans
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Data.Text.Lazy                 ( Text )

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

  routes
