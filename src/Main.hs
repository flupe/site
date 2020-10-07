{-# LANGUAGE LambdaCase #-}

module Main where

import qualified System.Process as Process
import System.Directory        (removePathForcibly)
import Control.Monad           (void, mapM_)
import Options.Applicative
import Lucid

import Common
import Templates
import Config (config, ropts, wopts, SiteConfig(title))

import qualified Posts
import qualified Projects
import qualified Visual
import qualified Readings

type ShowDrafts = Bool

data Cmd
    = Build ShowDrafts -- ^ Build the site
    | Deploy          -- ^ Deploy to the server
    | Clean           -- ^ Delete all artefacts
    deriving (Eq, Show)


cli :: Parser Cmd
cli = subparser $
      command "build"  (info (Build <$> switch (long "draft" <> short 'D' <> help "Display drafts"))
                             (progDesc "Build the site once" ))
   <> command "deploy" (info (pure Deploy) (progDesc "Server go brrr"      ))
   <> command "clean"  (info (pure Clean)  (progDesc "Delete all artefacts"))


main :: IO ()
main = customExecParser p opts >>= \case
    Deploy -> mapM_ Process.callCommand (deployCmd config)
    Clean  -> removePathForcibly        (outputDir config)
           >> removePathForcibly        (cacheFile config)
    Build showDrafts ->
        void $ runTask [] config (build showDrafts)
    where
        opts = info (cli <**> helper) $ fullDesc <> header desc
        p    = prefs showHelpOnEmpty
        desc = "acatalepsie & co"


build :: ShowDrafts -> Task IO String
build showDrafts = do
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
    Posts.build showDrafts
    Readings.build

