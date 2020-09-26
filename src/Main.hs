module Main where

import qualified Data.Yaml as Yaml
import Lucid

import Common
import Templates
import qualified Posts
import qualified Projects
import qualified Visual
import Config (config, ropts, wopts, SiteConfig(title))


main :: IO ()
main = achilleWith config do
    -- static assets
    match_ "assets/*" copyFile

    -- quid page
    match_ "./quid.rst" $
        compilePandoc 
        <&> toHtmlRaw
        <&> outerWith def {Config.title = "quid"}
        >>= saveFileAs (-<.> "html")

    Visual.build
    Projects.build
    Posts.build

    -- reading list
    matchFile "readings.yaml" $ readBS
        >>= (liftIO . Yaml.decodeThrow)
        <&> renderReadings
        >>= saveFileAs (-<.> "html")
