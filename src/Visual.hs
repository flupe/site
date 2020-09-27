module Visual (build) where

import Config
import Common
import Templates (outerWith, loading_)
import Lucid

build :: Task IO ()
build = do
    pictures <- match "visual/*" do
      copyFile
      runCommandWith (-<.> "thumb.png")
                     (\a b -> "convert -resize 740x " <> a <> " " <> b)
          <&> timestamped

    watch pictures $ match_ "./visual.rst" do
        intro <- compilePandoc
        write "visual.html" $ renderVisual intro (recentFirst pictures)

renderVisual :: Text -> [Timestamped FilePath] -> Html ()
renderVisual txt imgs =
    outerWith def {title = "visual"} do
        toHtmlRaw txt
        hr_ []
        section_ $ forM_ imgs \ (Timestamped _ p) ->
            figure_ $ img_ [ src_ (fromString p), loading_ "lazy" ]
