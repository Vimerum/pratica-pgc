module Utils where

import Codec.Picture

linspace :: Double -> Double -> Double -> [Double]
linspace begin end size = [begin,second..end]
    where
        second = begin + step
        step   = (end - begin) / (size - 1)

noModKernel :: [[Double]]
noModKernel = [[1]]

noModPixel :: (Double, Double, Double) -> (Double, Double, Double)
noModPixel pixel = pixel

scale :: [[Double]] -> Double
scale kernel = sum [sum x | x <- kernel]

tupleFromPixel :: PixelRGB8 -> (Double, Double, Double)
tupleFromPixel p@(PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

pixelFromTuple :: (Double, Double, Double) -> PixelRGB8
pixelFromTuple p = PixelRGB8 (floor r) (floor g) (floor b)
    where
        (r, g, b) = normalizePixel p

normalizePixel :: (Double, Double, Double) -> (Double, Double, Double)
normalizePixel (r, g, b) = (r', g', b')
    where
        r' = max 0 (min 255 r)
        g' = max 0 (min 255 g)
        b' = max 0 (min 255 b)

sumPixel :: [(Double, Double, Double)] -> (Double, Double, Double)
sumPixel [] = (0, 0, 0)
sumPixel (x@(r, g, b):xs) = (r+r', g+g', b+b')
    where
        x'@(r', g', b') = sumPixel xs

getNewPixel :: [[Double]] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
getNewPixel kernel img x y = sumPixel [sumPixel [(r * val, g * val, b * val) | ((r, g, b), val) <- zip linePix lineKernel] | (linePix, lineKernel) <- zip neighbors kernel]
    where
        tam       = length kernel
        neighbors = getNeighbors tam img x y

getNeighbors :: Int -> Image PixelRGB8 -> Int -> Int -> [[(Double, Double, Double)]]
getNeighbors tam img@(Image imageWidth imageHeight _) x y = [[tupleFromPixel (pixelAt img i j) | i <- [minX..maxX]] | j <- [minY..maxY]]
    where
        minX = min (max (x - (tam `div` 2)) 0) (imageWidth - tam)
        maxX = minX + tam
        minY = min (max (y - (tam `div` 2)) 0) (imageHeight - tam)
        maxY = minY + tam
