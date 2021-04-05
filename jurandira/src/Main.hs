module Main where

-- Reference: https://jaspervdj.be/posts/2014-11-27-comonads-image-processing.html

{-# LANGUAGE BangPattern #-}
import qualified Codec.Picture                as Juicy
import qualified Codec.Picture.Types          as M
import           Control.Applicative          ((<$>))
import           Control.Monad
import           Data.List                    (sort)
import           Data.Maybe                   (fromMaybe, maybeToList)
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import           Data.Vector.Storable.Mutable (STVector)
import           Data.Word                    (Word8)

boxImage :: Juicy.Image Juicy.PixelRGB8 -> M.MutableImage Int Juicy.PixelRGB8
boxImage image = do join (M.thawImage image)

unboxImage :: M.MutableImage Int Juicy.PixelRGB8 -> Juicy.Image Juicy.PixelRGB8
unboxImage = extract M.unsafeFreezeImage

readImage :: FilePath -> IO (M.MutableImage Int Juicy.PixelRGB8)
readImage filePath = do 
    errorImage <- Juicy.readImage filePath
    case errorImage of
      Right (Juicy.ImageRGB8 img) -> return (boxImage img)
      Right _                     -> error "readImage: unsopported format"
      Left err                    -> error $ "readImage:could not load image: " ++ err

writePng :: FilePath -> M.MutableImage Int Juicy.PixelRGB8 -> IO ()
writePng filePath = Juicy.writePng filePath . unboxImage

data FocusedImage a = FocusedImage 
    { piBoxedImage :: !(M.MutableImage Int a)
    , piX          :: !Int
    , piY          :: !Int
    }

instance Functor FocusedImage where
    fmap f (FocusedImage bi x y) = FocusedImage (fmap f bi) x y

focus :: M.MutableImage Int a -> FocusedImage a
focus bi
    | M.mutableImageWidth bi > 0 && M.mutableImageHeight bi > 0 = FocusedImage bi 0 0
    | otherwise                         =
        error "Cannot focus on empty images"

unfocus :: FocusedImage a -> M.MutableImage Int a
unfocus (FocusedImage bi _ _) = bi

class Functor w => Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b

instance Comonad FocusedImage where
    extract (FocusedImage bi x y) = do join (M.readPixel bi x y)

    extend f (FocusedImage bi@(M.MutableImage w h _) x y) = FocusedImage
        (M.MutableImage w h $ V.generate (w * h) $ \i ->
            let (y', x')  = i `divMod` w
             in f (FocusedImage bi x' y'))
        x y

neighbour :: Int -> Int -> FocusedImage a -> Maybe (FocusedImage a)
neighbour dx dy (FocusedImage bi x y) 
    | outOfBounds = Nothing
    | otherwise   = Just (FocusedImage bi x' y')
  where
      x'          = x + dx
      y'          = y + dy
      outOfBounds =
          x' < 0 || x' >= M.mutableImageWidth bi ||
          y' < 0 || y' >= M.mutableImageHeight bi

reduceNoise :: FocusedImage Juicy.PixelRGB8 -> Juicy.PixelRGB8
reduceNoise pixel = median [ extract p | x <- [-2, -1 .. 2], y <- [-2, -1 .. 2], p <- maybeToList (neighbour x y pixel)]

median :: [Juicy.PixelRGB8] -> Juicy.PixelRGB8
median xs
    | odd len   = sort xs !! (len `div` 2)
    | otherwise = case drop (len `div` 2 - 1) (sort xs) of
        ((Juicy.PixelRGB8 r1 g1 b1) : (Juicy.PixelRGB8 r2 g2 b2) : _) -> Juicy.PixelRGB8 (r1 `div` 2 + r2 `div` 2) (g1 `div` 2 + g2 `div` 2) (b1 `div` 2 + b2 `div` 2)
        _           -> error "median: empty list"
  where
    len = length xs

main :: IO()
main = do
    image <- readImage filePath
    writePng filePath' $ unfocus $ extend reduceNoise $ focus image
  where
      filePath  = "../res/baboon.png"
      filePath' = "../res/baboon-teste.png"
