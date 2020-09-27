---
title: Making a blog from scratch
---

# Making a blog from scratch

Let's see how to use **achille** for making a static site generator for a blog.
First we decide what will be the structure of our source directory.
We choose the following:

```bash
content
└── posts
    ├── 2020-04-13-hello-world.md
    ├── 2020-04-14-another-article.md
    └── 2020-05-21-some-more.md
```

We define the kind of metadata we want to allow in the frontmatter header
of our markdown files:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAny #-}

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data Meta = Meta
  { title :: Text
  } deriving (Generic, FromJSON)
```

This way we enfore correct metadata when retrieving the content of our files.
Every markdown file will have to begin with the following header for our
generator to proceed:

```markdown
---
title: My first blogpost!
---
```

Then we create a generic template for displaying a page, thanks to lucid:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}

import Lucid.Html5

renderPost :: Text -> Text -> Html ()
renderPost title content = wrapContent do
  h1_ $ toHtml title
  toHtmlRaw content

renderIndex :: [(Text, FilePath)] -> Html ()
renderIndex = wrapContent .
  ul_ . mconcat . map \(title, path) ->
    li_ $ a_ [href_ path] $ toHtml title

wrapContent :: Html () -> Html ()
wrapContent content = doctypehtml_ do
    head_ do
        meta_ [charset_ "utf-8"]
        title_ "my very first blog"

    body_ do
        header_ $ h1_ "BLOG"
        content_
```

We define a recipe for rendering every post:

```haskell
buildPosts :: Task IO [(String, FilePath)]
buildPosts =
  match "posts/*.md" do
    (Meta title, text) <- compilePandocMetadata
    saveFileAs (-<.> "html") (renderPost title text)
      <&> (title,)
```

We can define a simple recipe for rendering the index, given a list of posts:

```haskell
buildIndex :: [(Text, FilePath)] -> Task IO FilePath
buildIndex posts =
  save (renderIndex posts) "index.html"
```

Then, it's only a matter of composing the recipes and giving them to **achille**:

```haskell
main :: IO ()
main = achille do
  posts <- buildPosts
  buildIndex posts
```

And that's it, you now have a very minimalist incremental blog generator!
