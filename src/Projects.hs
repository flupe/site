module Projects (build) where

import Lucid
import Data.Char (digitToInt)
import qualified Data.Map.Strict as Map

import Common
import Types
import Config
import Templates

data Project = Project 
    { title    :: Text
    , subtitle :: Text
    , year     :: Text
    , labels   :: Map.Map Text Text
    } deriving (Generic, Eq, FromJSON, Binary)


build :: Task IO ()
build = do
    projects <- matchDir "projects/*/" buildProject

    watch projects $ match_ "./projects.rst" do
        intro <- compilePandocWith def wopts
        write "projects.html" (renderIndex intro projects)


buildProject :: Recipe IO a (Project, FilePath)
buildProject = do
    match "*" copyFile

    name     <- takeBaseName <$> getCurrentDir
    children <- buildChildren name

    watch children $ matchFile "index.*" do
        (meta, doc) <- readPandocMetadataWith ropts

        renderPandocWith wopts doc
            <&> renderProject meta children
            >>= saveFileAs (-<.> "html")

        (meta,) <$> getCurrentDir
    where
        buildChildren :: String -> Recipe IO a [(Text, FilePath)]
        buildChildren name = match "pages/*" do
            filepath <- getInput
            let (key, file) = getKey $ takeFileName filepath
            (TitledPage title _, doc) <- readPandocMetadataWith ropts
            renderPandocWith wopts doc
                <&> toHtmlRaw
                <&> outerWith (def {Config.title = title})
                >>= saveFileAs (const $ file -<.> "html")
                <&> (title,)


renderProject :: Project -> [(Text, FilePath)] -> Text -> Html ()
renderProject Project{..} children content =
    outerWith def { Config.title       = title
                  , Config.description = subtitle
                  } do
        header_ [class_ "project"] do
            div_ (img_ [src_ "logo.svg"])
            div_ do
                h1_ (toHtml title)
                p_  (toHtml subtitle)
                ul_ $ forM_ (Map.toList labels) \(k, v) -> li_ do
                    toHtml k <> ": "
                    if k == "repo" then
                        a_ [href_ $ "https://github.com/" <> v]
                            $ toHtml v
                    else toHtml v
        when (length children > 0) $
            ol_ [class_ "pages"] $ forM_ children \(title, path) ->
                li_ $ a_ [href_ (fromString path)] (toHtml title)
        toHtmlRaw content


renderIndex :: Text -> [(Project, FilePath)] -> Html ()
renderIndex intro projects =
    outerWith def { Config.title = "projects"
                  , Config.description = intro
                  } do
        toHtmlRaw intro
        ul_ [class_ "projects"] $ forM_ projects projectLink
    where
        projectLink :: (Project, FilePath) -> Html ()
        projectLink (Project{..}, path) =
            li_ $ a_ [href_ (fromString path)] do
                div_ $ img_ [src_ (fromString $ path </> "logo.svg")]
                div_ $ h2_ (toHtml title) >> p_  (toHtml subtitle)


getKey :: String -> (Int, String)
getKey xs = getKey' 0 xs
    where
        getKey' :: Int -> String -> (Int, String)
        getKey' k (x : xs) | x >= '0' && x <= '9' =
            getKey' (k * 10 +  digitToInt x) xs
        getKey' k ('-' : xs) = (k, xs)
        getKey' k xs         = (k, xs)
