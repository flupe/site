{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}

module Posts (build) where

import Data.Maybe (fromMaybe, mapMaybe)

import Common
import Page
import Config
import Templates


build :: Task IO ()
build = do
    posts <- reverse <$> sort <$> match "posts/*" do
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

    watch visible $ match_ "index.rst" do
        -- render index
        compilePandoc
            <&> renderIndex visible
            >>= saveFileAs (-<.> "html")

        -- build atom feed
        --
