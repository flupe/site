module Projects (build) where

import Data.Char        (digitToInt)

import Common
import Types
import Config
import Templates
import Lucid


getKey :: String -> (Int, String)
getKey xs = getKey' 0 xs
    where
        getKey' :: Int -> String -> (Int, String)
        getKey' k (x : xs) | x >= '0' && x <= '9' =
            getKey' (k * 10 +  digitToInt x) xs
        getKey' k ('-' : xs) = (k, xs)
        getKey' k xs         = (k, xs)


buildProject :: Recipe IO a (Project, FilePath)
buildProject = do
    match "*" copyFile

    name     <- takeBaseName <$> getCurrentDir
    children <- buildChildren name

    watch children $ matchFile "index.*" do
        (meta, doc) <- readPandocMetadataWith ropts
        renderPandocWith wopts doc <&> renderProject meta children
                         >>= saveFileAs (-<.> "html")
            >> (meta,) <$> getCurrentDir
    where
        buildChildren :: String -> Recipe IO a [(String, FilePath)]
        buildChildren name = match "pages/*" do
            filepath <- getInput
            let (key, file) = getKey $ takeFileName filepath
            (TitledPage title _, doc) <- readPandocMetadataWith ropts
            renderPandocWith wopts doc
                <&> toHtmlRaw
                <&> outerWith (def {Config.title = fromString title})
                >>= saveFileAs (const $ file -<.> "html")
                <&> (title,)

            -- sorted = sortBy (\(_, x, _, _, _) (_, y, _, _, _) -> compare x y) children

            -- match "pages/*" do
            --     renderPandocWith wopts doc
            --         <&> outerWith (def {title = name})
            --         >>= saveFileAs (const $ file -<.> "html")
            --         <&> (name,)
            --         -}


build :: Task IO ()
build = do
    projects <- matchDir "projects/*/" buildProject

    watch projects $ match_ "./projects.rst" do
        txt <- compilePandocWith def wopts
        write "projects.html" $ renderProjects txt projects
