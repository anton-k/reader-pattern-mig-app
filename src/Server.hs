module Server
  ( server
  , Env(..)
  , module X
  ) where

import Mig.Json
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
  "api/v1" /.
    [ "save" /. handleSave
    , "get/message" /. handleGetById
    , "list/tag" /. handleGetByTag
    , "toggle-logs" /. handleToggleLogs
    ]
  where
    handleSave :: Body SaveRequest -> Post (App Env) (Resp SaveResponse)
    handleSave (Body req) = Send $
      fmap ok $ withApp (.save) $ Save.handle req

    handleGetById :: Capture "message-id" MessageId -> Get (App Env) (RespOr ApiError Message)
    handleGetById (Capture messageId) = Send $
      withApp (.getMessage) $
        either (bad status400) ok <$> GetMessage.handle messageId

    handleGetByTag :: Capture "tag" Tag -> Get (App Env) (Resp [Message])
    handleGetByTag (Capture tag) = Send $
      fmap ok $ withApp (.listTag) $ ListTag.handle tag

    handleToggleLogs :: Post (App Env) (Resp ())
    handleToggleLogs = Send $
      fmap ok $ withApp (.toggleLogs) ToggleLog.handle
