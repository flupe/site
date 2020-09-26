{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Feed where

import Data.Text hiding (map)
import Data.XML.Types as XML
import qualified Data.Text.Lazy as Lazy
import Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export (textFeed)

import Common
import Types

class Reifiable a where
    toEntry :: a -> Atom.Entry

instance Reifiable Project where
    toEntry :: Project -> Atom.Entry
    toEntry (Project {title, subtitle}) =

toFeed :: Reifiable a => [a] -> Atom.Feed
toFeed items =
    ( Atom.nullFeed
        "https://acatalepsie.fr/atom.xml"
        (Atom.TextString "acatalepsie")
        "2017-08-01"
    )
    { Atom.feedEntries = map toEntry items
    }
