module Posts (build) where

import Data.Aeson.Types (FromJSON)
import Data.Binary      (Binary, put, get)
import Data.Time        (UTCTime, defaultTimeLocale)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (rfc822DateFormat, formatTime)
import GHC.Generics
import Lucid

import Text.Atom.Feed   as Atom
import Text.Feed.Types  (Feed(..))
import Text.Feed.Export (textFeed)

import Common
import Config (ropts, wopts)
import qualified Config
import Templates

--  metadata used for parsing YAML headers
data PostMeta = PostMeta
    { title       :: Text
    , draft       :: Maybe Bool
    , description :: Maybe Text
    } deriving (Generic, Eq, Show, FromJSON)

data Post = Post
    { postTitle       :: Text
    , postDate        :: UTCTime
    , postDraft       :: Bool
    , postDescription :: Maybe Text
    , postContent     :: Text
    , postPath        :: FilePath
    } deriving (Generic, Eq, Show, Binary)

instance IsTimestamped Post where timestamp = postDate


buildPost :: FilePath -> Task IO Post
buildPost src = do
    copyFile src
    (PostMeta title draft desc, pandoc) <- readPandocMetadataWith ropts src
    content <- renderPandocWith wopts pandoc

    pure (renderPost title src content)
        >>= write (src -<.> "html")
        <&> Post title (timestamp src) (fromMaybe False draft) Nothing content

toDate :: UTCTime -> String
toDate = formatTime defaultTimeLocale rfc822DateFormat

build :: Bool -> Task IO ()
build showDrafts = do
    posts <- match "posts/*" buildPost
             <&> filter (\p -> not (postDraft p) || showDrafts)
             <&> recentFirst

    watch posts $ match_ "index.rst" \src -> do
        compilePandoc src
            <&> renderIndex posts
            >>= write (src -<.> "html")

        now <- liftIO getCurrentTime
        let (Just feed) = textFeed (AtomFeed $ postsToFeed now posts)
        write "atom.xml" feed

    where
        postsToFeed now posts =
            ( Atom.nullFeed
                "https://acatalepsie.fr/atom.xml"
                (Atom.TextString "acatalepsie")
                "2017-08-01")
            { Atom.feedEntries = postToEntry <$> posts
            , Atom.feedUpdated = fromString $ toDate now
            }

        postToEntry :: Post -> Atom.Entry
        postToEntry post =
            ( Atom.nullEntry (fromString $ postPath post)
                             (Atom.TextString $ postTitle post)
                             (fromString $ toDate $ postDate post))
            { Atom.entryContent = Just $ Atom.HTMLContent $ postContent post
            , Atom.entrySummary = Atom.HTMLString <$> postDescription post
            }


renderPost :: Text -> FilePath -> Text -> Html ()
renderPost title source content =
    outerWith def { Config.title = title } do
        h1_ $ toHtml title
        toLink source "View source"
        toHtmlRaw content


renderIndex :: [Post] -> Text -> Html ()
renderIndex posts content = 
    outer do
        toHtmlRaw content
        h2_ "Latest posts"
        ul_ [ id_ "pidx" ] $ forM_ posts \post ->
            li_ do
                span_ $ fromString $ showDate (postDate post)
                toLink (postPath post) (toHtml $ postTitle post)
