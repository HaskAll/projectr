{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Projectr.API where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant.API

type API =
  QuestionAPI :<|>
  Raw

type QuestionAPI =
  "question" :> Capture "title" Text :> Get '[JSON] Question

data Question = Question
  { title :: Text
  , body  :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON Question
instance FromJSON Question
