module Unsharp where

import Codec.Picture
import Control.Monad.ST
import Data.List
import Data.List.Split
import Data.Word (Word8)
import qualified Codec.Picture.Types as M

import Utils
import Edge

unsharp :: Image PixelRGB8 -> FilePath -> [String] -> IO ()
unsharp img path args = do
    let mode  = head args
    let tmp   = splitOn "." path
    let name  = intercalate "" (init tmp)
    let path' = name ++ "_unsharp_" ++ mode ++ "." ++ last tmp
    (savePngImage path' . ImageRGB8 . unsharpEffect mode) img

unsharpEffect :: String -> Image PixelRGB8 -> Image PixelRGB8
unsharpEffect mode img@(Image imageWidth imageHeight _) = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let kernel 
          | mode == "simple"     = unsharpKernel edgeKernel
          | mode == "sobel_hori" = unsharpKernel sobelHoriKernel
          | mode == "sobel_vert" = unsharpKernel sobelVertKernel
          | otherwise            = error "Mode desconhecido"
    let go x y
          | x >= imageWidth  = go 0 (y + 1)
          | y >= imageHeight = M.unsafeFreezeImage mimg
          | otherwise        = do
                writePixel mimg x y (pixelFromTuple $ getNewPixel kernel img x y)
                go (x + 1) y
    go 0 0


unsharpKernel :: [[Double]] -> [[Double]]
unsharpKernel kernel = [[x - y | (x, y) <- zip iLine kLine] | (iLine, kLine) <- zip identity kernel]
    where
        identity = identityKernel (length kernel)

identityKernel :: Int -> [[Double]]
identityKernel tam = [[getI x y | x <- [0..t]] | y <- [0..t]]
    where
        t    = tam - 1
        half = tam `div` 2
        getI x y
              | x == half && y == half  = 1
              | otherwise               = 0

edgeKernel :: [[Double]]
edgeKernel = [[0, 1, 0], [1, -4, 1], [0, 1, 0]]
