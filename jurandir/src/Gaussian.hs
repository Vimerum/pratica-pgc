module Gaussian where

import Codec.Picture
import Control.Monad.ST
import Data.List
import Data.List.Split
import Data.Word (Word8)
import qualified Codec.Picture.Types as M

import Utils

sigma :: Double
sigma = 1

gaussianKernelSize :: Double
gaussianKernelSize = 5

gaussian :: Image PixelRGB8 -> FilePath -> [String] -> IO ()
gaussian img path args = do
    let n     = read (head args) :: Int
    let tmp   = splitOn "." path
    let name  = intercalate "" (init tmp)
    let path' = name ++ "_gaussian_" ++ show n ++ "." ++ last tmp
    (savePngImage path' . ImageRGB8 . gaussianIter n) img

gaussianIter :: (Eq t, Num t) => t -> Image PixelRGB8 -> Image PixelRGB8
gaussianIter 1 = gaussianEffect
gaussianIter n = gaussianEffect . gaussianIter (n-1)

gaussianEffect :: Image PixelRGB8 -> Image PixelRGB8
gaussianEffect img@(Image imageWidth imageHeight _) = runST $ do
  mimg <- M.newMutableImage imageWidth imageHeight          -- Cria imagem não inicializada com tamanho imageWidth e imageHeight
  let go x y                                                -- Navega na imagem, vai para o pixel na posição (x, y)
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = M.unsafeFreezeImage mimg
        | otherwise        = do
            writePixel mimg x y
              (scalePixel $ getNewPixel gaussianKernel img x y)
            go (x + 1) y
  go 0 0

scalePixel :: (Double, Double, Double) -> PixelRGB8 
scalePixel (r, g, b) = PixelRGB8 r' g' b'
    where
        r' = floor (r / scale gaussianKernel)
        g' = floor (g / scale gaussianKernel)
        b' = floor (b / scale gaussianKernel)

gaussianFunction :: Double -> Double -> Double
gaussianFunction x y = lhs * rhs
    where
        lhs = 1 / (2 * pi * (sigma^2))
        rhs = exp (-a / b)
        a   = (x^2) + (y^2)
        b   = 2 * (sigma^2)

gaussianKernel :: [[Double]]
gaussianKernel = [[ item * f | item <- row] | row <- mat]
    where
        line = linspace (-2) 2 gaussianKernelSize
        mat  = [[ gaussianFunction x y | x <- line] | y <- line]
        f    = 1 / head (head mat)

