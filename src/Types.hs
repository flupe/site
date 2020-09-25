{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import GHC.Generics
import Data.Aeson.Types (FromJSON)
import Data.Binary (Binary, put, get)
import Data.Time.LocalTime (ZonedTime)
import Data.Binary.Instances.Time ()
import Data.Text   (Text)
import qualified Data.Map.Strict as Map


-- | Full project description
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


-- | Book description for the readings page
data Book = Book
    { title     :: Text
    , author    :: Text
    , rating    :: Maybe Int
    , completed :: Maybe ZonedTime
    } deriving (Generic, Show)

instance FromJSON Project
instance FromJSON TitledPage
instance FromJSON Book

instance Binary Project where
    put (Project t s y l g) = put t >> put s >> put y >> put l >> put g
    get = Project <$> get <*> get <*> get <*> get <*> get

instance Binary Book where
    put (Book t a r c) = put t >> put a >> put r >> put c
    get = Book <$> get <*> get <*> get <*> get
