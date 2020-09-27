{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}

module Templates where


import Data.Time (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (zonedTimeToUTC)
import qualified Data.Map.Strict as Map

import Achille.Internal.IO (AchilleIO)
import Achille.Writable as Writable
import Lucid
import Lucid.Base (makeAttribute)

import Types
import Common
import Config


instance AchilleIO m => Writable m (Html a) where
    write to = Writable.write to . renderBS

showDate :: UTCTime -> String
showDate = formatTime defaultTimeLocale "%b %d, %_Y"

loading_ :: Text -> Attribute
loading_ = makeAttribute "loading"

property_ :: Text -> Attribute
property_ = makeAttribute "property"

toLink :: FilePath -> Html () -> Html ()
toLink url = a_ [ href_ (fromString $ "/" <> url) ]


logo :: Html ()
logo = toHtmlRaw ("<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" height=\"19px\" width=\"29px\"><path d=\"M 2,2 A 5,5 0 0 1 7,7 L 7, 12 A 5, 5 0 0 1 2,17 M 7,7 A 5,5 0 0 1 12,2 L 22,2 A 5,5 0 0 1 27,7 L 27,12 A 5, 5 0 0 1 22,17 L 12,17\" style=\"stroke-width: 2; stroke-linecap: butt; stroke-linejoin: bevel; stroke: #fff\" fill=\"none\"/></svg>" :: Text)

outer :: Html () -> Html ()
outer = outerWith def

outerWith :: SiteConfig -> Html () -> Html ()
outerWith SiteConfig{title,..} content = doctypehtml_ do
    head_ do
        meta_ [ name_ "viewport"
              , content_ "width=device-width, initial-scale=1.0, user-scalable=yes"
              ]
        meta_ [ name_ "theme-color",  content_ "#000000" ]
        meta_ [ name_ "robots", content_ "index, follow" ]
        meta_ [ charset_ "utf-8" ]
        link_ [ rel_ "stylesheet",  href_ "/assets/theme.css" ]
        link_ [ rel_  "shortcut icon"
              , type_ "image/svg"
              , href_ "/assets/favicon.svg"
              ]
        link_ [ rel_ "alternate"
              , type_ "application/atom+xml"
              , href_ "/atom.xml"
              ]
        meta_ [ property_ "og:title", content_ title ]
        meta_ [ property_ "og:type", content_ "website" ]
        meta_ [ property_ "og:image", content_ image ]
        meta_ [ property_ "og:description", content_ description ]
        title_ $ toHtml title

    body_ do
        header_ [ id_ "hd" ] $ section_ do
            a_ [ href_ "/" ] $ logo
            section_ $ nav_ do
                a_ [ href_ "/projects.html" ] "Projects"
                a_ [ href_ "/visual.html"   ] "Visual"
                a_ [ href_ "/readings.html" ] "Readings"
                a_ [ href_ "/quid.html"     ] "Quid"
                a_ [ href_ "/atom.xml"      ] "Feed"

        main_ content

        footer_ [ id_ "ft" ] do
            "flupe 2020 · "
            a_ [ href_ "https://creativecommons.org/licenses/by-nc/2.0/" ]
                "CC BY-NC 2.0"
            " · "
            a_ [ href_ "https://instagram.com/ba.bou.m/" ] "instagram"
            " · "
            a_ [ href_ "/atom.xml" ] "feed"
