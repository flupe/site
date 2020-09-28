---
title: Making a blog from scratch
---

# Making a blog from scratch

In this tutorial we'll see how to use **achille** for a simple blog generator.

## Content structure

The first step is to settle on the content structure.
For a blog, we will simply store each article in a separate markdown file,
inside the `posts/` folder.

```bash
content
└── posts
    ├── 2020-04-13-hello-world.md
    ├── 2020-04-14-another-article.md
    └── 2020-05-21-some-more.md
```

We require every article to have a title in their Front Matter header.
We also give them the possibility to specify a summary.

```md
---
title: Hello World!
draft: true
---

This is my first article on *this* blog.
It is powered by [achille](https://acatalepsie.fr/projects/achille).
```


## HTML template

Then we figure out how we want to render our blog posts. We need to produce
HTML, and achille doesn't care how we do it, so we are free to use any library
that suits us. My personal favorite is `lucid` so we'll use that here.

```hs
{-# LANGUAGE BlockArguments, OverloadedStrings #-}

import Lucid
import Data.Text (Text)

template :: Html () -> Html ()
template content = doctypehtml_ do
    head_ do
        meta_ [charset_ "utf-8"]
        title_ "Jenna's Weblog"
    body_ do
        header_ "Jenna's Weblog"
        content
        footer_ "© Jenna 2020"
```

We also tell achille how `Html ()` should be written on disk.

```hs
import Achille.Writable as Writable (Writable)

instance Writable (Html ()) where
    write to = Writable.write to . renderBS
```

## Processing articles

Now let's define what kind of information is associated with an article.

```hs
{-# LANGUAGE DeriveGeneric, DeriveAny #-}

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON)
import Data.Binary  (Binary)
import Data.Time    (UTCTime)

data PostMeta = PostMeta
  { title   :: Text
  , summary :: Maybe Text
  , draft   :: Maybe Bool
  } deriving (Generic, Eq, FromJSON)

data Post = Post
  { postTitle   :: Text
  , postSummary :: Maybe Text
  , postIsDraft :: Bool
  , postPath    :: FilePath
  } deriving (Generic, Eq, Binary)

renderPost :: Post -> Text -> Html ()
renderPost post content = template $
  article_ do
    header_ $ h1 (toHtml $ postTitle post)
    toHtmlRaw content
```

Because we derive `FromJSON` for `PostMeta`, we are now able to load a Front
Matter header and convert it directly to a value of type `PostMeta`. **Correct
metadata is thus enforced**. `Post` is a datatype containing all the processed
information about an article, and we derive `Binary` so that we can cache it later.
Notice how we do not store the article content here, there's no use.

Now we simply need a recipe for reading an article, processing it and rendering
the appropriate HTML output file:

```hs
import System.FilePath ((-<.>))
import Data.Maybe      (fromMaybe)

processPost :: Recipe IO FilePath Post
processPost = do
    outputPath  <- (-<.> "html") <$> getInput
    (meta, doc) <- readPandoc

    let post = Post { postTitle   = title meta
                    , postSummary = summary meta
                    , postIsDraft = fromMaybe False (draft meta)
                    , postPath    = outputPath
                    }

    renderPandoc doc <&> renderPost post >>= write outputPath

    return post
```

## Rendering the index

Ok, we now have a recipe for building individual articles, however we want to
be able to display them on the index. We need to filter out drafts. And we need
to sort them from most recent to oldest.

```hs
import Control.Monad (forM_, mapM_)

processPosts :: Task IO ()
processPosts = do
  posts <- match "posts/*.md" processPost
  let visible = filter (not postIsDraft) posts
  watch visible $ write "index.html" (renderIndex visible)

renderIndex :: [Post] -> Html ()
renderIndex posts = do
  h2_ "Latest articles"
  ul_ $ forM_ posts \ post -> li_ do
    a_ [href_ (fromString $ postPath post)] (toHtml $ postTitle post)
    forM_ (postSummary post) \summary -> p_ $ toHtml summary
```

## Wrapping things up

Finally, we can forward this task to the top-level achille runner:

```hs
main :: IO ()
main = achille processPosts
```

Compile into an executable `blog` and run the command `blog build`. Hold and
behold, if you look into the `_site/` folder, there should be:

```bash
_site
├── index.html
└── posts
    ├── 2020-04-13-hello-world.html
    ├── 2020-04-14-another-article.html
    └── 2020-05-21-some-more.html
```

What's more, your generator is **incremental**. Modify a single article, and
trigger a rebuild.

```bash
$ touch -m content/posts/2020-04-13-hello-world.md
$ blog build
```

You should see that only this article is rebuilt. And because we haven't
actually changed neither the title nor the summary, the index hasn't been rebuilt.
Magic!
