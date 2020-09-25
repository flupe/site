{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Templates where

import Control.Monad (forM_, when)

import Text.Blaze.Internal         as I
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A
import Data.Dates.Types (DateTime(..), months, capitalize)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (zonedTimeToUTC)

import Types
import Common
import Config
import qualified Data.Map.Strict as Map

showDate :: DateTime -> String
showDate (DateTime y m d _ _ _) = month <> " " <> show d <> ", " <> show y
    where month = take 3 $ capitalize (months !! (m - 1))

loading :: AttributeValue -> Attribute
loading = I.customAttribute "loading"

property :: AttributeValue -> Attribute
property = I.customAttribute "property"

toLink :: FilePath -> Html -> Html
toLink url = H.a ! A.href (fromString $ "/" <> url)


renderIndex :: [Timestamped (String, FilePath)] -> Html -> Html
renderIndex posts content = 
    outer do
        content
        H.h2 "Latest notes"
        H.ul ! A.id "pidx" $ forM_ posts \(Timestamped d (title, src)) ->
            H.li do
                H.span $ fromString $ showDate d
                toLink src (fromString title)

renderPost :: String -> FilePath -> Html -> Html
renderPost title source content =
    outerWith def { Config.title = fromString title } do
        H.h1 $ fromString title
        toLink source "View source"
        content

renderVisual :: Html -> [Timestamped FilePath] -> Html
renderVisual txt imgs =
    outer do
        txt
        H.hr
        H.section $ forM_ imgs \ (Timestamped _ p) ->
            H.figure $ H.img ! A.src    (fromString p)
                             ! loading "lazy"

renderProject :: Project -> [(String, FilePath)] -> Html -> Html
renderProject (project@Project{title,..}) children content =
    outerWith def { Config.title       = fromString title
                  , Config.description = fromString subtitle
                  } do
        H.header ! A.class_ "project" $ do
            H.div $ H.img ! A.src "logo.svg"
            H.div do
                H.h1 $ fromString $ title
                H.p $ fromString $ subtitle
                H.ul $ forM_ (Map.toList labels) \(k, v) -> H.li do
                    fromString k <> ": "
                    if k == "repo" then
                        H.a ! A.href (fromString $ "https://github.com/" <> v)
                            $ fromString v
                    else fromString v
        when (length children > 0) $
            H.ol ! A.class_ "pages" $ forM_ children \(t,l) ->
                H.li $ H.a ! A.href (fromString l) $ (fromString t)
        content

renderReadings :: [Book] -> Html
renderReadings books =
    outerWith def { Config.title       = "readings"
                  , Config.description = "books I've read"
                  } do
        H.table ! A.class_ "books" $
            forM_ books \ Book {title,author,rating,completed} ->
                H.tr do
                    H.td $ toHtml title 
                    H.td $ toHtml author
                    H.td $ fromString $ case rating of
                        Just r  -> replicate r '★'
                        Nothing -> "·"
                    H.td $ fromString $ case completed of
                        Just d  -> formatTime defaultTimeLocale "%m/%0Y"
                                   $ zonedTimeToUTC d
                        Nothing -> "·"

renderProjects :: Html -> [(Project, FilePath)] -> Html
renderProjects txt paths =
    outer do
        txt
        H.ul ! A.class_ "projects" $ do
            forM_ paths \(Project {title,..}, link) -> H.li $ H.a ! A.href (fromString link) $ do
                H.div $ H.img ! A.src (fromString $ link </> "logo.svg")
                H.div do
                    H.h2 $ fromString title
                    H.p  $ fromString subtitle
                    -- H.ul $ forM_ (Map.toList $ Types.labels project) \(k, v) ->
                    --     H.li $ (fromString k <> ": " <> fromString v)

logo :: Html
logo = preEscapedString "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" height=\"19px\" width=\"29px\"><path d=\"M 2,2 A 5,5 0 0 1 7,7 L 7, 12 A 5, 5 0 0 1 2,17 M 7,7 A 5,5 0 0 1 12,2 L 22,2 A 5,5 0 0 1 27,7 L 27,12 A 5, 5 0 0 1 22,17 L 12,17\" style=\"stroke-width: 2; stroke-linecap: butt; stroke-linejoin: bevel; stroke: #fff\" fill=\"none\"/></svg>"

outer :: Html -> Html
outer = outerWith def

outerWith :: SiteConfig -> Html -> Html
outerWith SiteConfig{title,..} content = H.docTypeHtml do
    H.head do
        H.meta ! A.name "viewport"
               ! A.content "width=device-width, initial-scale=1.0, user-scalable=yes"
        H.meta ! A.name "theme-color" ! A.content "#000000"
        H.meta ! A.name "robots" ! A.content "index, follow"
        H.meta ! charset "utf-8"
        H.link ! A.rel "stylesheet" ! A.href "/assets/theme.css"

        -- OpenGraph
        H.meta ! property "og:title"
               ! A.content (textValue title)

        H.meta ! property "og:type"
               ! A.content "website"

        H.meta ! property "og:image"
               ! A.content (textValue image)

        H.meta ! property "og:description"
               ! A.content (textValue description)

        H.title $ toHtml title

    H.body do
        H.header ! A.id "hd" $ H.section do
            H.a ! A.href "/" $ logo
            H.section $ H.nav do
                H.a ! A.href "/projects.html" $ "Projects"
                H.a ! A.href "/visual.html"   $ "Visual"
                H.a ! A.href "/readings.html" $ "Readings"
                H.a ! A.href "/quid.html"     $ "Quid"

        H.main content

        H.footer ! A.id "ft" $ do
            "flupe 2020 · "
            H.a ! A.href "https://creativecommons.org/licenses/by-nc/2.0/" $ "CC BY-NC 2.0"
            " · "
            H.a ! A.href "https://instagram.com/ba.bou.m/" $ "instagram"

