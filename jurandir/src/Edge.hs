module Edge where

import Codec.Picture
import Control.Monad.ST
import Data.List
import Data.List.Split
import Data.Word (Word8)
import qualified Codec.Picture.Types as M

import Utils
import Gaussian (gaussianIter)
import BlackWhite (blackWhiteEffect)

edge :: Image PixelRGB8 -> FilePath -> [String] -> IO()
edge img path args = do
    let out     = head args
    let tmp     = splitOn "." path
    let name    = intercalate "" (init tmp)
    let path'   = name ++ "_edge." ++ last tmp
    let path2   = name ++ "_edge_hori." ++ last tmp
    let path3   = name ++ "_edge_vert." ++ last tmp
    let gausImg = gaussianIter 1 img
    let bwImg   = blackWhiteEffect gausImg
    let horiImg = sobel sobelHoriKernel bwImg
    let vertImg = sobel sobelVertKernel bwImg
    case out of
      "simple"  -> (savePngImage path' . ImageRGB8 . sumImages horiImg) vertImg
      "all"     -> do
          (savePngImage path' . ImageRGB8 . sumImages horiImg) vertImg
          (savePngImage path2 . ImageRGB8) horiImg
          (savePngImage path3 . ImageRGB8) vertImg
      _         -> putStrLn "Opção inválida"

sobel :: [[Double]] -> Image PixelRGB8 -> Image PixelRGB8
sobel kernel img@(Image imageWidth imageHeight _) = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = M.unsafeFreezeImage mimg
            | otherwise        = do
                writePixel mimg x y (pixelFromTuple $ getNewPixel kernel img x y)
                go (x + 1) y
    go 0 0

sumImages :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
sumImages lImg rImg@(Image imageWidth imageHeight _) = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = M.unsafeFreezeImage mimg
            | otherwise        = do
                let lPixel = pixelAt lImg x y
                let rPixel = pixelAt rImg x y
                writePixel mimg x y (combine lPixel rPixel)
                go (x + 1) y
    go 0 0

combine :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
combine p1 p2 = PixelRGB8 r' g' b'
    where
        (r1, g1, b1) = tupleFromPixel p1
        (r2, g2, b2) = tupleFromPixel p2
        r'           = floor (sqrt (r1^2 + r2^2))
        g'           = floor (sqrt (g1^2 + g2^2))
        b'           = floor (sqrt (b1^2 + b2^2))

sobelHoriKernel :: [[Double]]
sobelHoriKernel = [[1, 0, -1],[2, 0, -2], [1, 0, -1]]

sobelVertKernel :: [[Double]]
sobelVertKernel = [[1, 2, 1], [0, 0, 0], [-1, -2, -1]]
