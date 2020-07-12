{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import GHC.Generics
import Data.Aeson.Types (FromJSON)
import Data.Binary (Binary, put, get)
import Data.Default
import qualified Data.Map.Strict as Map

data Project = Project 
    { title    :: String
    , subtitle :: String
    , year     :: String
    , labels   :: Map.Map String String
    , gallery  :: Maybe Bool
    } deriving (Generic, Eq, Show)

data TitledPage = TitledPage
    { title       :: String
    , description :: Maybe String
    } deriving (Generic, Eq, Show)

instance FromJSON Project
instance FromJSON TitledPage

instance Binary Project where
    put (Project t s y l g) = put t >> put s >> put y >> put l >> put g
    get = Project <$> get <*> get <*> get <*> get <*> get


data SiteConfig = SiteConfig
    { title       :: String
    , description :: String
    , image       :: String
    }

instance Default SiteConfig where
    def = SiteConfig
        { title       = "sbbls"
        , description = "my personal web space, for your enjoyment"
        , image       = "https://acatalepsie.fr/assets/card.png"
        }
