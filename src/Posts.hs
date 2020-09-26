{-# LANGUAGE DeriveGeneric #-}

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
    } deriving (Generic, Eq, Show)

data Post = Post
    { postTitle       :: Text
    , postDate        :: UTCTime
    , postDraft       :: Bool
    , postDescription :: Maybe Text
    , postContent     :: Text
    , postPath        :: FilePath
    } deriving (Generic, Eq, Show)

instance FromJSON PostMeta
instance IsTimestamped Post where timestamp = postDate
instance Binary Post where
    put (Post t d dr desc content path) =
        put t >> put d >> put dr >> put desc >> put content >> put path
    get = Post <$> get <*> get <*> get <*> get <*> get <*> get


buildPost :: Recipe IO FilePath Post
buildPost = do
    src <- copyFile
    (PostMeta title draft desc, pandoc) <- readPandocMetadataWith ropts
    content <- renderPandocWith wopts pandoc

    pure (renderPost title src content)
        >>= saveFileAs (-<.> "html")
        <&> Post title (timestamp src) (fromMaybe False draft) Nothing content

toDate :: UTCTime -> String
toDate = formatTime defaultTimeLocale rfc822DateFormat

build :: Task IO ()
build = do
    posts <- match "posts/*" buildPost
             <&> filter (not . postDraft)
             <&> recentFirst

    watch posts $ match_ "index.rst" do
        compilePandoc
            <&> renderIndex posts
            >>= saveFileAs (-<.> "html")

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
