---
title: achille
subtitle: A Haskell library for building static site generators
year: "2020"
labels:
  repo: flupe/achille
  license: MIT
---

**achille** [aʃil] is a tiny Haskell library for building your very own **static site
generator**. It is in spirit a direct successor to [Hakyll][Hakyll].

## Motivation

Static site generators (SSG) have proven to be very useful tools for easily
generating static websites from neatly organised content files. Most of them
support using **markup languages** like markdown for writing content, and offer
**incremental compilation** so that updating a website stays **fast**,
regardless of its size. However, most SSGs are very opinionated about how you
should manage your content. As soon as your specific needs deviate slightly
from what your SSG supports, it becomes a lot more tedious.

This leads to many people writing their own personal static site generators
from scratch. This results in a completely personalised workflow, but without
good libraries it is a time-consuming endeavor, and incremental compilation is often
out of the equation as it is hard to get right.

This is where **achille** and [Hakyll][Hakyll] come in: they provide a *domain
specific language* embedded in Haskell to easily yet very precisely describe
how to build your site. Compile this description and **you get a full-fledged
static site generator with incremental compilation**, tailored specifically to
your needs.

[Hakyll]: https://jaspervdj.be/hakyll

### Why Hakyll is not enough

To provide incremental compilation, Hakyll relies on a global store, in which
all your *intermediate values* are stored. It is *your* responsibility to
populate it with *snapshots*. There are some severe limitations to this
approach:

- The store is **fundamentally untyped**, so **retrieving snapshots may fail at
  runtime** if you're not careful when writing your build rules. You may
  argue that's not very critical --- I think it shouldn't be possible in the
  first place. We are using a strongly typed language, so we shouldn't have
  to rely on flaky coercions at runtime to manipulate intermediate values.

- **Loading snapshots with glob patterns is awkward**. With Hakyll, *the*
  way to retrieve intermediate values is by querying the store,
  using glob patterns. This indirect way of managing values is very
  clumsy. In Haskell, the very purpose of variables is to store intermediate
  values, so we should only have to deal with plain old variables.

- **Dependencies are not explicit**. Because it relies on a global store for
  handling intermediate values, Hakyll has to make sure that the snaphots you
  want to load have been generated already. And because rules have no imposed
  order despite implicit inter-dependencies, Hakyll has to evaluate very
  carefully each rule, eventually pausing them to compute missing dependencies.
  This is very complex and quite frankly impressive, yet I believe we can strive
  for a simpler model of evaluation. If we used plain old variables to hold
  intermediate values, we simply would not be allowed to refer to an undefined
  variable.

There are other somewhat debatable design decisions:

- In Hakyll, every rule will produce an output file, and only one, if you're
  restricting yourself to the API they provide. I argue
  such a library should not care whether a rule produces any output on the
  filesystem. Its role is merely to know *if the rule must be executed*. Because of
  this requirement, producing multiple outputs from the same file is a tad
  cumbersome.
- Because Hakyll stores many content files directly in the store, the resulting
  cache is *huge*. This is unnecessary, the files are right here in the content
  directory.
- Hakyll uses a *lot* of abstractions --- `Compiler`, `Item`, `Rule`, `RuleSet`
  --- whose purpose is not obvious to a newcomer.
- It defines monads to allow the convenient `do` notation to be used, but
  disregards completely the very benefit of using monads --- it composes!

### Other tools

As always when thinking I am onto something, I jumped straight into code
and forgot to check whether there were alternatives. By fixating on Hakyll, I did not
realize many people have had the same comments about the shortcomings of Hakyll
and improved upon it. Therefore, it's only after building most of **achille**
in a week that I realized there were many
other similar tools available, namely: [rib][rib], [slick][slick], [Pencil][pencil] &
[Lykah][lykah].

[rib]:    https://rib.srid.ca/
[slick]:  https://hackage.haskell.org/package/slick
[pencil]: http://elbenshira.com/pencil/
[lykah]:  https://hackage.haskell.org/package/Lykah

Fortunately, I still believe **achille** is a significant improvement over these libraries.

- As far as I can tell, **pencil** does not provide incremental generation.
  It also relies on a global store, no longer untyped but very
  restrictive about what you can store. It implements its own templating language.
- Likewise, no incremental generation in **Lykah**.
  Reimplements its own HTML DSL rather than use *lucid*.
  Very opinionated, undocumented and unmaintained.
- **rib** and **slick** are the most feature-complete of the lot.
  They both provide a minimalist web-focused interface over the very powerful build system
  [Shake][Shake].

[Shake]: https://shakebuild.com/

## How achille works

In **achille** there is a single abstraction for reasoning about build rules:
`Recipe m a b`.  A **recipe** of type `Recipe m a b` will produce a value of type
`m b` given some input of type `a`.
Conveniently, if `m` is a monad then **`Recipe m a` is a monad** too, so
you can retrieve the output of a recipe to reuse it in another recipe.

*(Because of caching, a recipe is **not** just a Kleisli arrow)*


```haskell
-- the (>>=) operator, restricted to recipes
(>>=) :: Monad m => Recipe m a b -> (b -> Recipe m a c) -> Recipe m a c
```

With only this, **achille** tackles every single one of the limitations highlighted above.

- Intermediate values are plain old Haskell variables.

  ```haskell
  renderPost     :: Recipe IO FilePath Post
  buildPostIndex :: [Post] -> Recipe a ()

  renderPosts :: Task IO ()
  renderPosts = do
      posts <- match "posts/*" renderPost
      buildPostIndex posts
  ```

  See how a correct ordering of build rules is enforced by design: you can only
  use an intermediate value once the recipe it is originating from has been
  executed.

  Note: a **task** is a recipe that takes no input.

  ```haskell
  type Task m = Recipe m ()
  ```

- **achille** does not care what happens during the execution of a recipe.
  It only cares about the input and return type of the recipe --- that is, the
  type of intermediate values.
  In particullar, **achille** does not expect every recipe to produce a file,
  and lets you decide when to actually write on the filesystem.

  For example, it is very easy to produce multiple versions of a same source file:

  ```haskell
  renderPage :: Recipe IO FilePath FilePath
  renderPage = do
      -- Copy the input file as is to the output directory
      copyFile

      -- Render the input file with pandoc,
      -- then save it to the output dir with extension ".html"
      compilePandoc >>= saveTo (-<.> "html")
  ```

Once you have defined the recipe for building your site, you forward
this description to **achille** in order to get a command-line interface for
your generator, just as you would using Hakyll:

```haskell
buildSite :: Task IO ()

main :: IO ()
main = achille buildSite
```

Assuming we compiled the file above into an executable called `site`, running
it gives the following output:

```bash
$ site
A static site generator for fun and profit

Usage: site COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Build the site once
  deploy                   Server go brrr
  clean                    Delete all artefacts
```

That's it, you now have your very own static site generator!

### Caching

So far we haven't talked about caching and incremental builds.
Rest assured: **achille produces generators with robust incremental
builds** for free. To understand how this is done, we can simply look at the
definition of `Recipe m a b`:

```haskell
-- the cache is simply a lazy bytestring
type Cache = ByteString

newtype Recipe m a b = Recipe (Context a -> m (b, Cache))
```

In other words, when a recipe is run, it is provided a **context** containing
the input value, **a current cache** *local* to the recipe, and some more
information. The IO action is executed, and we update the local cache with the
new cache returned by the recipe. We say *local* because of how composition of
recipes is handled internally. When the *composition* of two recipes (made with
`>>=` or `>>`) is being run, we retrieve two bytestrings from the local cache
and feed them as local cache to both recipes respectively. Then we gather the two updated
caches, join them and make it the new cache of the composition.

This way, a recipe is guaranteed to receive the same local cache it returned
during the last run, *untouched by other recipes*. And every recipe is free to
dispose of this local cache however it wants.

As a friend noted, **achille** is "just a library for composing memoized
computations".

----

#### High-level interface

Because we do not want the user to carry the burden of updating the cache
manually, **achille** comes with many utilies for common operations, managing
the cache for us under the hood. Here is an exemple highlighting how we keep
fine-grained control over the cache at all times, while never having to
manipulate it directly.

Say you want to run a recipe for every file maching a glob pattern, *but do
not care about the output of the recipe*. A typical exemple would be to copy
every static asset of your site to the output directory. **achille** provides
the `match_` function for this very purpose:

```haskell
match_ :: Glob.Pattern -> Recipe FilePath b -> Recipe a ()
```

We would use it in this way:

```haskell
copyAssets :: Recipe a ()
copyAssets = match_ "assets/*" copyFile

main :: IO ()
main = achille copyAssets
```

Under the hood, `match_ p r` will cache every filepath for which the recipe was
run.  During the next run, for every filepath matching the pattern, `match_ p r` will
lookup the path in its cache. If it is found and hasn't been modified since,
then we do nothing for this path. Otherwise, the task is run and the filepath
added to the cache.

Now assume we do care about the output of the recipe we want to run on every filepath.
For example if we compile every blogpost, we want to retrieve each blogpost's title and
the filepath of the compiled `.html` file. In that case, we can use the
built-in `match` function:

```haskell
match :: Binary b
      => Glob.Pattern -> Recipe FilePath b -> Recipe a [b]
```

Notice the difference here: we expect the type of the recipe output `b` to have
an instance of `Binary`, **so that we can encode it in the cache**. Fortunately,
many of the usual Haskell types have an instance available. Then we can do:

```haskell
data PostMeta = PostMeta { title :: Text }
renderPost :: Text -> Text -> Text
renderIndex :: [(Text, FilePath)] -> Text

buildPost :: Recipe FilePath (Text, FilePath)
buildPost = do
  (PostMeta title, pandoc) <- compilePandocMeta
  renderPost title pdc & saveAs (-<.> "html")
    <&> (title,)

buildPost :: Recipe a [(Text, FilePath)]
buildPosts = match "posts/*.md" buildPost

buildIndex :: [(Text, FilePath)] -> Recipe 
```

#### Shortcomings

The assertion *"A recipe will always receive the same cache between two runs"*
can only violated in the two following situations:

- There is **conditional branching in your recipes**, and more specifically,
  **branching for which the branch taken can differ between runs**.

  For example, it is **not** problematic to do branching on the extension of a file,
  as the same path will be taken each execution.

  But assuming you want to parametrize by some boolean value for whatever reason,
  whose value you may change between runs, then because the two branches will
  share the same cache, every time the boolean changes, the recipe will start
  from an inconsistent cache so it will recompute from scratch, and overwrite
  the existing cache.

  ```haskell
  buildSection :: Bool -> Task IO ()
  buildSection isProductionBuild =
    if isProductionBuild then
      someRecipe
    else
      someOtherRecipe
  ```

  Although I expect few people ever do this kind of conditional branching for
  generating a static site, **achille** still comes with combinators for branching.
  You can use `if` in order to keep two separate caches for the two branches:

  ```haskell
  if :: Bool -> Recipe m a b -> Recipe m a b -> Recipe m a b
  ```

  The previous example becomes:

  ```haskell
  buildSection :: Bool -> Task IO ()
  buildSection isProductionBuild =
    Achille.if isProductionBuild
      someRecipe
      someOtherRecipe
  ```

### No runtime failures

All the built-in cached recipes **achille** provides are implemented carefully
so that **they never fail in case of cache corruption**. That is, in the
eventuality of failing to retrieve the desired values from the cache, our
recipes will automatically recompute the result from the input, ignoring the
cache entirely. To make sure this is indeed what happens, every cached recipe
in **achille** has been tested carefully (not yet really, but it is on the todo
list).

This means the only failures possible are those related to poor content
formatting from the user part: missing frontmatter fields, watching files 
that do not exist, etc. All of those are errors are gracefully reported to the
user.

### Parallelism

**achille** could very easily support parallelism for free, I just didn't take
the time to make it a reality.


## Making a blog from scratch

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

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data Meta = Meta
  { title :: Text
  } deriving (Generic)

instance FromJSON Meta
```

This way we enfore correct metadata when retrieving the content of our files.
Every markdown file will have to begin with the following header for our
generator to proceed:

```markdown
---
title: Something about efficiency
---
```

Then we create a generic template for displaying a page, thanks to lucid:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}

import Lucid.Html5

renderPost :: Text -> Text -> Html a
renderPost title content = wrapContent do
  h1_ $ toHtml title
  toHtmlRaw content

renderIndex :: [(Text, FilePath)] -> Html a
renderIndex = wrapContent .
  ul_ . mconcat . map \(title, path) ->
    li_ $ a_ [href_ path] $ toHtml title

wrapContent :: Html a -> Html a
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

## Recursive recipes

It is very easy to define recursive recipes in **achille**. This allows us to
traverse and build tree-like structures, such as wikis.

For example, given the following structure:

```bash
content
├── index.md
├── folder1
│   └── index.md
└── folder2
    ├── index.md
    ├── folder21
    │   └── index.md
    ├── folder22
    │   └── index.md
    └── folder23
        ├── index.md
        ├── folder231
        │   └── index.md
        ├── folder222
        │   └── index.md
        └── folder233
            └── index.md
```

We can generate a site with the same structure and in which each index page has
links to its children:

```haskell
renderIndex :: PageMeta -> [(PageMeta, FilePath)] -> Text -> Html

buildIndex :: Recipe IO a (PageMeta, FilePath)
buildIndex = do
    children <- walkDir

    matchFile "index.*" do
        (meta, text) <- compilePandoc
        renderIndex meta children text >>= save (-<.> "html")
        return $ (meta,) <$> getInput

walkDir :: Recipe IO a [(PageMeta, FilePath)]
walkDir = matchDir "*/" buildIndex

main :: IO ()
main = achille buildIndex
```

## Forcing the regeneration of output

Currently, **achille** doesn't track what files a recipe produces in the output
dir. This means you cannot ask for things like *"Please rebuild
output/index.html"*.

That's because we make the assumption that the output dir is untouched between
builds. The only reason I can think of for wanting to rebuild a specific page
is if the template used to generate it has changed.
But in that case, the template is *just another input*.
So you can treat it as such by putting it in your content directory and doing
the following:

```haskell
import Templates.Index (renderIndex)

buildIndex :: Task IO ()
buildIndex = 
    watchFile "Templates/Index.hs" $ match_ "index.*" do
        compilePandoc <&> renderIndex >>= write "index.html"
```

This way, **achille** will automatically rebuild your index if the template has
changed!

While writing these lines, I realized it would be very easy for **achille**
to know which recipe produced which output file,
so I might just add that. Still, it would still require you to ask for an output
file to be rebuilt if a template has changed. With the above pattern, it is
handled automatically!
