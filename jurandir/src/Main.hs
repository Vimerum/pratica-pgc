module Main where

-- Referencias:
--      http://hackage.haskell.org/package/JuicyPixels
--      https://www.stackbuilders.com/tutorials/haskell/image-processing/

import System.Environment (getArgs)
import Codec.Picture
import Control.Monad.ST
import Data.List
import Data.List.Split
import Data.Word (Word8)
import qualified Codec.Picture.Types as M

import Utils
import Gaussian
import Grayscale
import Edge
import Unsharp

data Effect = Gaussian | Grayscale | Edge | Unsharp

main :: IO ()
main = do
    (path:effect:_) <- getArgs
    eimg <- readImage path
    let path'         = getPath effect path
    let kernel        = getKernel effect
    let pixelModifier = getPixelModifier effect
    case eimg of
      Left err              -> putStrLn ("\nNão foi possivel ler a imagem. Lembre-se de usar uma imagem PNG.\n\nErro: " ++ err)
      Right (ImageRGB8 img) -> (savePngImage path' . ImageRGB8 . applyKernel kernel pixelModifier) img
      Right _               -> putStrLn "Pixel com formato inesperado"

effectFromString :: String -> Maybe Effect
effectFromString "gaussian"  = Just Gaussian
effectFromString "grayscale" = Just Grayscale
effectFromString "edge"      = Just Edge
effectFromString "unsharp"   = Just Unsharp
effectFromString _           = Nothing

getPath :: String -> FilePath -> String
getPath effectString path = do
    let effect = effectFromString effectString
    case effect of
      Just Gaussian  -> gaussianPath path
      Just Grayscale -> grayscalePath path
      Just Edge      -> edgePath path
      Just Unsharp   -> unsharpPath path
      Nothing        -> path

getKernel :: String -> [[Double]]
getKernel effectString = do
    let effect  = effectFromString effectString 
    case effect of
      Just Gaussian  -> gaussianKernel
      Just Edge      -> Edge.edgeKernel 
      Just Unsharp   -> unsharpKernel
      Just _         -> noModKernel
      Nothing        -> noModKernel

getPixelModifier :: String -> ((Double, Double, Double) -> (Double, Double, Double))
getPixelModifier effectString = do
    let effect = effectFromString effectString
    case effect of
      Just Gaussian  -> gaussianPixelMod
      Just Grayscale -> grayscalePixelMod
      Just _         -> noModPixel
      Nothing        -> noModPixel

applyKernel :: [[Double]] -> ((Double, Double, Double) -> (Double, Double, Double)) -> Image PixelRGB8 -> Image PixelRGB8
applyKernel kernel pixelModifier img@(Image imageWidth imageHeight _) = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight 
    let go x y
          | x >= imageWidth = go 0 (y + 1)
          | y >= imageHeight = M.unsafeFreezeImage mimg
          | otherwise = do
              writePixel mimg x y (pixelFromTuple $ pixelModifier $ getNewPixel kernel img x y)
              go (x + 1) y 
    go 0 0
