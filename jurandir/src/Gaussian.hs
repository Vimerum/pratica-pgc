module Gaussian where

import Data.List
import Data.List.Split

import Utils

sigma :: Double
sigma = 1

gaussianKernelSize :: Double
gaussianKernelSize = 5

gaussianPath :: String -> String 
gaussianPath path = name ++ "_gaussian." ++ last tmp
    where
        tmp = splitOn "." path
        name = intercalate "" (init tmp)

gaussianPixelMod:: (Double, Double, Double) -> (Double, Double, Double)
gaussianPixelMod (r, g, b) = (r', g', b')
    where
        r' = r / scale gaussianKernel
        g' = g / scale gaussianKernel
        b' = b / scale gaussianKernel

gaussianKernel :: [[Double]]
gaussianKernel = [[ item * f | item <- row] | row <- mat]
    where
        line = linspace (-2) 2 gaussianKernelSize
        mat  = [[ gaussianFunction x y | x <- line] | y <- line]
        f    = 1 / head (head mat)

gaussianFunction :: Double -> Double -> Double
gaussianFunction x y = lhs * rhs
    where
        lhs = 1 / (2 * pi * (sigma^2))
        rhs = exp (-a / b)
        a   = (x^2) + (y^2)
        b   = 2 * (sigma^2)
