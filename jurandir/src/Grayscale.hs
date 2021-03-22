module Grayscale where
    
import Data.List
import Data.List.Split

grayscalePath :: String -> String
grayscalePath path = name ++ "_grayscale." ++ last tmp
    where
        tmp = splitOn "." path
        name = intercalate "" (init tmp)

grayscalePixelMod :: (Double, Double, Double) -> (Double, Double, Double)
grayscalePixelMod (r, g, b) = (avg, avg, avg)
    where
        avg       = (r + g + b) / 3
