module Unsharp where

import Data.List
import Data.List.Split

unsharpPath :: String -> String 
unsharpPath path = name ++ "_unsharp." ++ last tmp 
    where
        tmp = splitOn "." path
        name = intercalate "" (init tmp)

unsharpKernel :: [[Double]]
unsharpKernel = [[x - y | (x, y) <- zip iLine kLine] | (iLine, kLine) <- zip identity edgeKernel]
    where
        identity = identityKernel (length edgeKernel)

identityKernel :: Int -> [[Double]]
identityKernel tam = [[getI x y | x <- [0..t]] | y <- [0..t]]
    where
        t    = tam - 1
        half = tam `div` 2
        getI x y
              | x == half && y == half  = 1
              | otherwise               = 0

edgeKernel :: [[Double]]
edgeKernel = [[0, 1, 0], [1, -4, 1], [0, 1, 0]]
