module Visual (build) where

import Config
import Common
import Templates (outerWith, loading_)
import Lucid


thumbWidth :: Int
thumbWidth = 710


build :: Task IO ()
build = do
    pictures <- match "visual/*" \src -> do
        copyFile src
        callCommandWith
            (\a b -> "convert " <> a <> " " <> b)
            (-<.> "webp")
            src
        src' <- toAbsolute src
        size <- (resized . read)
                <$> readCommand "identify" ["-ping", "-format", "(%w, %h)", src']
        callCommandWith
            (\a b -> "convert -resize 710x " <> a <> " " <> b)
            (-<.> "thumb.webp")
            src
            <&> timestamped
            <&> fmap (size,) 

    watch pictures $ match_ "./visual.rst" \src -> do
        intro <- compilePandoc src
        write "visual.html" $ renderVisual intro (recentFirst pictures)
    where
        resized :: (Int, Int) -> (Int, Int)
        resized (width, height) =
            (thumbWidth, round $ fi height * fi thumbWidth / fi width)
            where
                fi :: Int -> Float
                fi = fromIntegral


renderVisual :: Text -> [Timestamped ((Int, Int), FilePath)] -> Html ()
renderVisual txt imgs =
    outerWith def {title = "visual"} do
        toHtmlRaw txt
        hr_ []
        section_ [class_ "visual"] $
            forM_ imgs \ (Timestamped _ ((width, height), p)) ->
                figure_ $ a_ [href_ (fromString (replaceExtensions p "webp"))] $ img_
                    [ src_ (fromString p)
                    , width_ (fromString $ show width)
                    , height_ (fromString $ show height)
                    , loading_ "lazy" ]
