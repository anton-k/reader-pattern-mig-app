module Main (main) where

import Mig qualified

import qualified Control.Immortal as Immortal
import Control.Concurrent (threadDelay)
import Server
import Types
import DI.Log
import App.DI.Db
import App.DI.Log
import App.DI.Setup
import App.DI.Time
import App.State
import Config

import Server.GetMessage qualified as GetMessage
import Server.ListTag    qualified as ListTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog

main :: IO ()
main = runServer =<< readConfig

runServer :: Config -> IO ()
runServer config = do
  -- init mutable shared state
  verboseVar <- newVerboseVar

  -- init interfaces
  ilog  <- initLog verboseVar
  idb   <- initDb config.db
  itime <- initTime config.time
  let
    isetup = initSetup verboseVar

    -- init local envirnoments
    env =
      Env
        { save       = Save.Env (addLogContext "api.save" ilog) idb.save itime
        , getMessage = GetMessage.Env idb.getMessage (addLogContext "api.get-message" ilog)
        , listTag    = ListTag.Env idb.listTag (addLogContext "api.list-tag" ilog)
        , toggleLogs = ToggleLog.Env (addLogContext "api.toggle-log" ilog) isetup
        }

  runImmortal $ do
    ilog.logInfo $ "Start server on http://localhost:" <> display config.port
    Mig.runServer config.port (Mig.renderServer server env)

------------------------------------------------------------
-- utils

runImmortal :: IO () -> IO ()
runImmortal act = do
  -- start an immortal thread
  _thread <- Immortal.create $ \ _thread -> act

  -- in the main thread, sleep until interrupted
  -- (e.g. with Ctrl-C)
  forever $ threadDelay maxBound
