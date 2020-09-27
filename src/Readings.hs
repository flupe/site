module Readings (build) where

import qualified Data.Yaml as Yaml
import Lucid
import Common
import Config
import Templates


data Book = Book
    { title     :: Text
    , author    :: Text
    , rating    :: Maybe Int
    } deriving (Generic, Show, FromJSON)


build :: Recipe IO () FilePath
build = matchFile "readings.yaml" $
    readBS
        >>= (liftIO . Yaml.decodeThrow)
        <&> renderReadings
        >>= saveFileAs (-<.> "html")


renderReadings :: [Book] -> Html ()
renderReadings books =
    outerWith def { Config.title       = "readings"
                  , Config.description = "books I've read"
                  } do
        table_ [ class_ "books" ] $
            forM_ books \Book {title, author, rating} ->
                tr_ do
                    td_ (toHtml title)
                    td_ (toHtml author)
                    td_ (toHtml $ fromMaybe "." $ flip replicate '★' <$> rating)
