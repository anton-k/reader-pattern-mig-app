{-# Language UndecidableInstances #-}
-- | Reader pattern monad
module App
  ( App
  , runApp
  , withApp
  , module X
  ) where

import Control.Monad.IO.Class as X
import Control.Monad.Reader as X
import Control.Monad.Except as X
import Control.Monad.Catch as X (MonadThrow(..), MonadCatch(..))
import Mig.Json.IO

newtype App env a = App (ReaderT env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader env
    , MonadIO
    , MonadThrow
    , MonadCatch
    , HasServer
    )

runApp :: App env a -> env -> IO a
runApp (App a) env = runReaderT a env

withApp :: (env' -> env) -> App env a -> App env' a
withApp f (App r) = App (withReaderT f r)
