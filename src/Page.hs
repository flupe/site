{-# LANGUAGE DeriveGeneric #-}

module Page where

import GHC.Generics
import Data.Aeson.Types (FromJSON)

data Page = Page
    { title  :: String
    , draft  :: Maybe Bool
    } deriving (Generic, Eq, Show)

instance FromJSON Page
