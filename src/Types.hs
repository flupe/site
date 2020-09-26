{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Time.LocalTime (ZonedTime)
import Data.Binary.Instances.Time ()
import qualified Data.Map.Strict as Map
import Common

-- | Full project description
data Project = Project 
    { title    :: String
    , subtitle :: String
    , year     :: String
    , labels   :: Map.Map String String
    , gallery  :: Maybe Bool
    } deriving (Generic, Eq, Show, FromJSON, Binary)


data TitledPage = TitledPage
    { title       :: String
    , description :: Maybe String
    } deriving (Generic, Eq, Show, FromJSON, Binary)


-- | Book description for the readings page
data Book = Book
    { title     :: Text
    , author    :: Text
    , rating    :: Maybe Int
    , completed :: Maybe ZonedTime
    } deriving (Generic, Show, FromJSON)
