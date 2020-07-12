{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Functor
import Data.Maybe      (fromMaybe, mapMaybe)
import Control.Monad   (when)
import System.FilePath
import Text.Pandoc.Options

import Achille
import Achille.Recipe.Pandoc

import Page
import Templates
import Types


config :: Config
config = def
    { deployCmd  = Just "rsync -avzzr _site/ --chmod=755 pi@192.168.0.45:/var/www/html"

    -- by making everything absolute you can run the command from anywhere
    , contentDir = "/home/flupe/dev/acatalepsie/content"
    , outputDir  = "/home/flupe/dev/acatalepsie/_site"
    , cacheFile  = "/home/flupe/dev/acatalepsie/.cache"
    }


-- pandoc options
ropts :: ReaderOptions
ropts = def
    { readerExtensions =
          enableExtension Ext_smart githubMarkdownExtensions
    }

wopts :: WriterOptions
wopts = def
    { writerHTMLMathMethod = KaTeX ""
    }


buildProject :: Recipe IO a (Project, FilePath)
buildProject = do
    name <- takeBaseName <$> getCurrentDir

    -- task $ match_ "*" copyFile
    match "*" copyFile

    children <- buildChildren

    watch children $ matchFile "index.*" do
        (meta, doc) <- readPandocMetadataWith ropts
        renderPandocWith wopts doc <&> renderProject meta children
                         >>= saveFileAs (-<.> "html")
            >> (meta,) <$> getCurrentDir
    where
        buildChildren :: Recipe IO a [(String, FilePath)]
        buildChildren = match "pages/*" do
            (TitledPage title _, doc) <- readPandocMetadataWith ropts
            renderPandocWith wopts doc
                <&> outerWith (def {title = title})
                >>= saveFileAs ((-<.> "html") . takeFileName)
                <&> (title,)


main :: IO ()
main = achilleWith config do
    match_ "assets/*" copyFile

    -----------
    -- QUID

    match_ "./quid.rst" $
        compilePandoc <&> outerWith def {title = "quid"}
        >>= saveFileAs (-<.> "html")

    -----------
    -- VISUAL

    pictures <- match "visual/*" do
      copyFile
      runCommandWith (-<.> "thumb.png")
                     (\a b -> "convert -resize 740x " <> a <> " " <> b)
          <&> timestamped

    watch pictures $ match_ "./visual.rst" do
        txt <- compilePandoc
        write "visual.html" $ renderVisual txt (recentFirst pictures)

    -------------
    -- PROJECTS

    projects <- matchDir "projects/*/" buildProject

    watch projects $ match_ "./projects.rst" do
        debug "rendering project index"
        txt <- compilePandocWith def wopts
        write "projects.html" $ renderProjects txt projects

    ------------------
    -- POSTS & INDEX

    posts <- match "posts/*" do
        src <- copyFile
        (Page title d, pdc) <- readPandocMetadataWith ropts

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
