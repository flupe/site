{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE TupleSections         #-}

import Data.Functor    ((<&>))
import Data.Maybe      (fromMaybe, mapMaybe)
import System.FilePath ((-<.>))
import Text.Pandoc.Options

import Achille
import Achille.Recipe.Pandoc

import Page
import Templates

config :: Config
config = def
    { deployCmd = Just "rsync -avzzr _site/ --chmod=755 flupe@duckduck.me:/var/www/acatalepsie"
    } 

main :: IO ()
main = achilleWith config do
    match_ "assets/*" copyFile
    match_ "./quid.rst" $ compilePandoc <&> outer >>= saveFileAs (-<.> "html")

    -----------
    -- VISUAL

    pictures <- match "visual/*/*" do
        copyFile
        runCommandWith (-<.> "thumb.png")
                       (\a b -> "convert -resize 740x " <> a <> " " <> b)
            <&> timestamped

    watch pictures $ match_ "./visual.rst" do
        txt <- compilePandoc
        write "visual.html" $ renderVisual txt (recentFirst pictures)

    -------------
    -- PROJECTS

    projects <- matchDir "projects/*/" do
        task $ match_ "*" copyFile
        matchFile "index.*" do
            (meta, doc) <- readPandocMetadataWith ropts
            renderPandoc doc <&> renderProject meta
                             >>= saveFileAs (-<.> "html")
                >> (meta,) <$> getCurrentDir

    watch projects $ match_ "./projects.rst" do
        debug "rendering project index"
        txt <- compilePandocWith def wopts
        write "projects.html" $ renderProjects txt projects

    ------------------
    -- POSTS & INDEX

    posts <- match "posts/*" do
        src <- copyFile
        (Page title d, pdc) <- readPandocMetadata

        renderPandocWith wopts pdc
            <&> renderPost title src
            >>= saveFileAs (-<.> "html")
            <&> (d,) . (title,)
            <&> timestampedWith (timestamp . snd . snd)

    let visible = mapMaybe
            (\(Timestamped d (dr, p)) ->
                 if fromMaybe False dr then Nothing
                                       else Just $ Timestamped d p) posts

    watch (take 10 visible) $ match_ "./index.rst" do
        debug "rendering index"
        compilePandoc
            <&> renderIndex visible
            >>= saveFileAs (-<.> "html")

ropts :: ReaderOptions
ropts = def { readerExtensions = enableExtension Ext_smart githubMarkdownExtensions }

wopts :: WriterOptions
wopts = def { writerHTMLMathMethod = KaTeX "" }
