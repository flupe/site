module Main where

import Lucid

import Common
import Templates
import Config (config, ropts, wopts, SiteConfig(title))

import qualified Posts
import qualified Projects
import qualified Visual
import qualified Readings


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
    Readings.build
