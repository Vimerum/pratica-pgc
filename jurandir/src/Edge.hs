module Edge where

import Data.List
import Data.List.Split

edgePath path = name ++ "_edge." ++ last tmp
    where
        tmp = splitOn "." path
        name = intercalate "" (init tmp)

edgeKernel :: [[Double]]
edgeKernel = [[0, -4, -2],[4, 0, -4], [2, 4, 0]]
