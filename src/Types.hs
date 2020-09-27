module Types where

import Common

data TitledPage = TitledPage
    { title       :: Text
    , description :: Maybe Text
    } deriving (Generic, Eq, FromJSON, Binary)


