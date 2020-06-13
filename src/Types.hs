{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson.Types (FromJSON)
import Data.Binary (Binary, put, get)
import qualified Data.Map.Strict as Map

data Project = Project 
    { title    :: String
    , subtitle :: String
    , year     :: String
    , labels   :: Map.Map String String
    , gallery  :: Maybe Bool
    } deriving (Generic, Eq, Show)

instance FromJSON Project
instance Binary Project where
    put (Project t s y l g) = put t >> put s >> put y >> put l >> put g
    get = Project <$> get <*> get <*> get <*> get <*> get

