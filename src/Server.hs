module Server
  ( server
  , Env(..)
  , module X
  ) where

import Mig.Json
import Data.Bifunctor
import Server.GetMessage qualified as GetMessage
import Server.ListTag    qualified as ListTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog

import Api as X
import Types

-- | Service environment by methods
data Env = Env
  { save        :: Save.Env
  , getMessage  :: GetMessage.Env
  , listTag     :: ListTag.Env
  , toggleLogs  :: ToggleLog.Env
  }

-- | Mig server for the app
server :: Server (App Env)
server =
  "api" /. "v1" /.
     mconcat
       [ "save" /. handleSave
       , "get" /. "message" /. handleGetById
       , "list" /. "tag" /. handleGetByTag
       , "toggle-logs" /. handleToggleLogs
       ]
  where
    handleSave :: Body SaveRequest -> Post (App Env) SaveResponse
    handleSave (Body req) = Post $ withApp (.save) $ Save.handle req

    handleGetById :: Capture MessageId -> Get (App Env) (Either (Error ApiError) Message)
    handleGetById (Capture messageId) = Get $ withApp (.getMessage) $
      first (Error status400) <$> GetMessage.handle messageId

    handleGetByTag :: Capture Tag -> Get (App Env) [Message]
    handleGetByTag (Capture tag) = Get $ withApp (.listTag) $ ListTag.handle tag

    handleToggleLogs :: Post (App Env) ()
    handleToggleLogs = Post $ withApp (.toggleLogs) ToggleLog.handle
