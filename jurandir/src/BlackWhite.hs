module BlackWhite where
    
import Codec.Picture
import Control.Monad.ST
import Data.List
import Data.List.Split
import Data.Word (Word8)
import qualified Codec.Picture.Types as M

import Utils

blackWhite :: Image PixelRGB8 -> FilePath -> [String] -> IO()
blackWhite img path args = do
    let tmp   = splitOn "." path
    let name  = intercalate "" (init tmp)
    let path' = name ++ "_blackwhite." ++ last tmp
    (savePngImage path' . ImageRGB8 . blackWhiteEffect) img

blackWhiteEffect :: Image PixelRGB8 -> Image PixelRGB8
blackWhiteEffect img@(Image imageWidth imageHeight _) = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y
          | x >= imageWidth  = go 0 (y + 1)
          | y >= imageHeight = M.unsafeFreezeImage mimg
          | otherwise        = do
              writePixel mimg x y (getAvgPixel (pixelAt img x y))
              go (x + 1) y
    go 0 0

getAvgPixel :: PixelRGB8 -> PixelRGB8
getAvgPixel p = PixelRGB8 avg avg avg
    where
        avg       = floor ((r + g + b) / 3)
        (r, g, b) = tupleFromPixel p
