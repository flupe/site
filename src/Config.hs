module Config (config, ropts, wopts, SiteConfig(..), def) where

import Data.Default
import Data.Text (Text)
import Text.Pandoc.Options as Pandoc
import Achille (Config(..))


config :: Achille.Config
config = def
    { deployCmd  = Just "rsync -avzzr _site/ --chmod=755 acatalepsie:/var/www/html"
    , contentDir = root <> "content"
    , outputDir  = root <> "_site"
    , cacheFile  = root <> ".cache"
    } where root = "/home/flupe/dev/acatalepsie/"


ropts :: Pandoc.ReaderOptions
ropts = def { readerExtensions = pandocExtensions }

wopts :: Pandoc.WriterOptions
wopts = def { writerHTMLMathMethod = KaTeX "" }


data SiteConfig = SiteConfig
    { title       :: Text
    , description :: Text
    , image       :: Text
    }

instance Default SiteConfig where
    def = SiteConfig
        { title       = "sbbls"
        , description = "my personal web space, for your enjoyment"
        , image       = "https://acatalepsie.fr/assets/card.png"
        }
