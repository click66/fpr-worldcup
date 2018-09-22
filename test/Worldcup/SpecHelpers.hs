module Worldcup.SpecHelpers where

import Worldcup.Result
import Worldcup.Team

-- Enumerates all values of a given Enum datatype into a list
enumerateAll :: Enum a => [a]
enumerateAll = enumFrom (toEnum 0)

-- Generates a list of all possible results
results :: [Result]
results = [ Win, Loss, Draw ]

-- Checks if all elements in a given list of integers are equal
allEqual :: [Int] -> Bool
allEqual []        = True
allEqual (x:xs)
    | x /= head xs = False
    | otherwise    = allEqual xs