module Visual (build) where

import Common
import Templates

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
