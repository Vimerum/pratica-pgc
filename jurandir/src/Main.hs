module Main where

-- Referencias:
--      http://hackage.haskell.org/package/JuicyPixels
--      https://www.stackbuilders.com/tutorials/haskell/image-processing/

import Codec.Picture
import System.Environment (getArgs)

import Gaussian
import BlackWhite
import Edge
import Unsharp

data Effect = Gaussian | BlackWhite | Edge | Unsharp

fromEffect :: String -> Maybe Effect
fromEffect "gaussian"   = Just Gaussian
fromEffect "blackwhite" = Just BlackWhite
fromEffect "edge"       = Just Edge
fromEffect "unsharp"    = Just Unsharp
fromEffect _            = Nothing

main :: IO ()
main = do
    (path:effect:args) <- getArgs
    eimg <- readImage path
    case eimg of
      Left err              -> putStrLn ("\nNão foi possivel ler a imagem. Lembre-se de usar uma imagem PNG.\n\nErro: " ++ err)
      Right (ImageRGB8 img) -> getEffect (fromEffect effect) img path args
      Right _               -> putStrLn "Pixel com formato inesperado"


getEffect :: Maybe Effect -> Image PixelRGB8 -> FilePath -> [String] -> IO()
getEffect effect = do
    case effect of
      Just Gaussian     -> gaussian
      Just BlackWhite   -> blackWhite
      Just Edge         -> edge
      Just Unsharp      -> unsharp
      Nothing           -> effectError

effectError :: a -> b -> c -> IO()
effectError _ _ _ = putStrLn "Efeito invalido. Opções válidas:\n\
        \\tGaussian filter: [path] gaussian [number-of-iterations]\n\
        \\tBlack and White: [path] blackwhite\n\
        \\tEdge detection:  [path] edge [simple/al]l\n\
        \\tUnsharp masking: [path] unsharp [simple/sobel_hori/sobel_vert]"
