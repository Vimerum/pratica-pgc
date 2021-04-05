module CloneRGB where

-- Reference: https://jaspervdj.be/posts/2014-11-27-comonads-image-processing.html

{-# LANGUAGE BangPattern #-}
import qualified Codec.Picture                as Juicy
import           Control.Applicative          ((<$>))
import           Data.List                    (sort)
import           Data.Maybe                   (fromMaybe, maybeToList)
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import           Data.Word                    (Word8)

data BoxedImage a = BoxedImage
    { biWidth    :: !Int
    , biHeight   :: !Int
    , biData     :: !(V.Vector a)
    }

instance Functor BoxedImage where
    fmap f (BoxedImage w h d) = BoxedImage w h (fmap f d)

boxImage :: Juicy.Image Juicy.PixelRGB8 -> BoxedImage Juicy.PixelRGB8
boxImage image = BoxedImage 
    { biWidth  = Juicy.imageWidth image
    , biHeight = Juicy.imageHeight image
    , biData   = V.fromList $ packPixels $ V.toList $ VG.convert (Juicy.imageData image)
    }

unboxImage :: BoxedImage Juicy.PixelRGB8 -> Juicy.Image Juicy.PixelRGB8
unboxImage boxedImage = Juicy.Image
    { Juicy.imageWidth = biWidth boxedImage
    , Juicy.imageHeight = biHeight boxedImage
    , Juicy.imageData = VG.convert $ V.fromList $ unpackPixels $ V.toList $ VG.convert (biData boxedImage)
    }

packPixels :: [Juicy.Pixel8] -> [Juicy.PixelRGB8]
packPixels (r:g:b:xs) = Juicy.PixelRGB8 r g b : packPixels xs
packPixels _          = []

unpackPixels :: [Juicy.PixelRGB8] -> [Juicy.Pixel8]
unpackPixels (p@(Juicy.PixelRGB8 r g b):xs) = r : g : b : unpackPixels xs
unpackPixels _                              = []

readImage :: FilePath -> IO (BoxedImage Juicy.PixelRGB8)
readImage filePath = do 
    errorImage <- Juicy.readImage filePath
    case errorImage of
      Right (Juicy.ImageRGB8 img) -> return (boxImage img)
      Right _                     -> error "readImage: unsopported format"
      Left err                    -> error $ "readImage:could not load image: " ++ err

writePng :: FilePath -> BoxedImage Juicy.PixelRGB8 -> IO ()
writePng filePath = Juicy.writePng filePath . unboxImage

data FocusedImage a = FocusedImage 
    { piBoxedImage :: !(BoxedImage a)
    , piX          :: !Int
    , piY          :: !Int
    }

instance Functor FocusedImage where
    fmap f (FocusedImage bi x y) = FocusedImage (fmap f bi) x y

focus :: BoxedImage a -> FocusedImage a
focus bi
    | biWidth bi > 0 && biHeight bi > 0 = FocusedImage bi 0 0
    | otherwise                         =
        error "Cannot focus on empty images"

unfocus :: FocusedImage a -> BoxedImage a
unfocus (FocusedImage bi _ _) = bi

class Functor w => Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b

instance Comonad FocusedImage where
    extract (FocusedImage bi x y) =
        biData bi V.! (y * biWidth bi + x)

    extend f (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage
        (BoxedImage w h $ V.generate (w * h) $ \i ->
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
          x' < 0 || x' >= biWidth bi ||
          y' < 0 || y' >= biHeight bi

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

runCloneRGB :: IO()
runCloneRGB = do
    image <- readImage filePath
    writePng filePath' $ unfocus $ extend reduceNoise $ focus image
  where
      filePath  = "../res/baboon.png"
      filePath' = "../res/baboon-teste.png"
