module Clone where

-- Author: Jasper Van der Jeugt (jaspervdj)
-- https://jaspervdj.be/posts/2014-11-27-comonads-image-processing.html

{-# LANGUAGE BangPattern #-}
import qualified Codec.Picture       as Juicy
import           Control.Applicative ((<$>))
import           Data.List           (sort)
import           Data.Maybe          (fromMaybe, maybeToList)
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as VG
import           Data.Word           (Word8)

type Pixel = Word8

data BoxedImage a = BoxedImage
    { biWidth  :: !Int
    , biHeight :: !Int
    , biData   :: !(V.Vector a)
    }

instance Functor BoxedImage where
    fmap f (BoxedImage w h d) = BoxedImage w h (fmap f d)

boxImage :: Juicy.Image Juicy.Pixel8 -> BoxedImage Pixel
boxImage image = BoxedImage
    { biWidth  = Juicy.imageWidth image
    , biHeight = Juicy.imageHeight image
    , biData   = VG.convert (Juicy.imageData image)
    }

unboxImage :: BoxedImage Pixel -> Juicy.Image Juicy.Pixel8
unboxImage boxedImage = Juicy.Image
    { Juicy.imageWidth  = biWidth boxedImage
    , Juicy.imageHeight = biHeight boxedImage
    , Juicy.imageData   = VG.convert (biData boxedImage)
    }

readImage :: FilePath -> IO (BoxedImage Pixel)
readImage filePath = do
    errOrImage <- Juicy.readImage filePath
    case errOrImage of
      Right (Juicy.ImageY8 img) -> return (boxImage img)
      Right _                   -> error "readImage: unsupported format"
      Left err                  -> error $ "readImage: could not load image: " ++ err

writePng :: FilePath -> BoxedImage Pixel -> IO ()
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

reduceNoise1 :: FocusedImage Pixel -> Pixel
reduceNoise1 pixel = median [ extract p | x <- [-2, -1 .. 2], y <- [-2, -1 .. 2], p <- maybeToList (neighbour x y pixel)]

median :: Integral a => [a] -> a
median xs
    | odd len   = sort xs !! (len `div` 2)
    | otherwise = case drop (len `div` 2 - 1) (sort xs) of
        (x : y : _) -> x `div` 2 + y `div` 2
        _           -> error "median: empty list"
  where
    len = length xs

runClone :: IO ()
runClone = do
    image <- readImage filePath
    writePng filePath' $ unfocus $ extend reduceNoise1 $ focus image
  where
      filePath  = "../res/stairs.png"
      filePath' = "../res/stairs-teste.png"
