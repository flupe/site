---
title: How achille works
---

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
