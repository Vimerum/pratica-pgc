module Main where
-- Reference: https://jaspervdj.be/posts/2014-11-27-comonads-image-processing.html

{-# LANGUAGE BangPattern #-}
import qualified Codec.Picture                as Juicy
import qualified Codec.Picture.Types          as M
import           Control.Applicative          ((<$>))
import           Control.Monad
import           Control.Monad.Primitive      (PrimState, PrimMonad, primToIO)
import           Data.List                    (sort)
import           Data.Maybe                   (fromMaybe, maybeToList)
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word                    (Word8)

data BoxedImage a = BoxedImage
    { biWidth  :: !Int
    , biHeight :: !Int
    , biData   :: !(VSM.STVector (PrimState IO) a)
    }

instance Functor BoxedImage where
    fmap f (BoxedImage w h d) = undefined --BoxedImage w h (fmap f d)

boxImage :: Juicy.Image Juicy.PixelRGB8 -> BoxedImage Juicy.PixelRGB8
boxImage image = BoxedImage
    { biWidth  = Juicy.imageWidth image
    , biHeight = Juicy.imageHeight image
    , biData   = undefined
    }
--        where
--            (M.MutableImage w h _) = M.thawImage image

unboxImage = undefined
--unboxImage :: BoxedImage Juicy.PixelRGB8 -> Juicy.Image Juicy.PixelRGB8
--unboxImage (BoxedImage w h d) = M.generateImage (\x y -> VSM.read d (x + y * w)) w h

readImage :: FilePath -> IO (BoxedImage Juicy.PixelRGB8)
readImage filePath = do 
    errorImage <- Juicy.readImage filePath
    case errorImage of
      Right (Juicy.ImageRGB8 img) -> return (boxImage img)
      Right _                     -> error "readImage: unsupported format"
      Left err                    -> error $ "readImage: could not load image: " ++ err

--writeImage :: FilePath -> BoxedImage Juicy.PixelRGB8 -> IO ()
--writeImage filePath = Juicy.writePng filePath . M.freezeImage . unboxImage

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

class Functor w => ComonadIOa w where
    extract :: VSM.Storable a => w a -> IO a
    --extend :: (w a -> b) -> w a -> w b

instance ComonadIOa FocusedImage where
    extract (FocusedImage (BoxedImage w _ d) x y) = VSM.read d (x + y * w)

extend f (FocusedImage bi@(BoxedImage w h _) x y) = do 
    biD <- VSM.generate (w * h) $ \i ->
        let (y', x') = i `divMod` w
        in f (FocusedImage bi x' y')
    FocusedImage (BoxedImage w h biD) x y

writePng = undefined

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
reduceNoise = undefined
--reduceNoise pixel = median [ extract p | x <- [-2, -1 .. 2], y <- [-2, -1 .. 2], p <- maybeToList (neighbour x y pixel)]

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
    putStrLn "Hello"
    image <- readImage filePath
    writePng filePath' $ unfocus $ extend reduceNoise $ focus image
    putStrLn "End"
    where
        filePath  = "../res/baboon.png"
        filePath' = "../res/baboon-teste.png"
