module Visual (build) where

import Config
import Common
import Templates (outerWith, loading_)
import Lucid

build :: Task IO ()
build = do
    pictures <- match "visual/*" \src -> do
      copyFile src
      src' <- toAbsolute src
      size <- (resized . read)
              <$> readCommand "identify" ["-ping", "-format", "(%w, %h)", src']
      processMagick src
        <&> timestamped
        <&> fmap (size,) 

    watch pictures $ match_ "./visual.rst" \src -> do
        intro <- compilePandoc src
        write "visual.html" $ renderVisual intro (recentFirst pictures)

resized :: (Int, Int) -> (Int, Int)
resized (width, height) = (710, round $ fi height * 710.0 / fi width)
    where
        fi :: Int -> Float
        fi = fromIntegral

renderVisual :: Text -> [Timestamped ((Int, Int), FilePath)] -> Html ()
renderVisual txt imgs =
    outerWith def {title = "visual"} do
        toHtmlRaw txt
        hr_ []
        section_ $ forM_ imgs \ (Timestamped _ ((width, height), p)) ->
            figure_ $ img_
                [ src_ (fromString p)
                , width_ (fromString $ show width)
                , height_ (fromString $ show height)
                , loading_ "lazy" ]

processMagick :: FilePath -> Task IO FilePath
processMagick = callCommandWith
    (\a b -> "convert -resize 710x " <> a <> " " <> b)
    (-<.> "thumb.webp")
