-- | API for the service.
-- For simplicity it is in single module, but will be split on parts
-- by methods in real project.
module Api
  ( SaveRequest(..)
  , SaveResponse(..)
  , ApiError (..)
  ) where

import Deriving.Aeson
import Deriving.Aeson.Stock
import Types
import Mig (ToSchema)

data SaveRequest = SaveRequest
  { message :: Text
  , tags    :: [Tag]
  }
  deriving stock (Show, Generic)
  deriving (ToSchema)
  deriving (FromJSON, ToJSON) via Vanilla SaveRequest

newtype SaveResponse = SaveResponse MessageId
  deriving newtype (ToJSON, FromJSON, Show, Eq)

-- | we use plain Text but in real app it's going to be
-- a Sum type which describes possible exceptions of our app
newtype ApiError = ApiError Text
  deriving newtype (ToJSON, FromJSON, Show, Eq)
