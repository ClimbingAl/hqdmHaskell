
module HqdmInspection
( 
    howmanyNodes
) where

import HqdmLib (HqdmTriple, subject )

howmanyNodes :: (String -> Bool) -> [String] -> Int
howmanyNodes p xs = sum [ 1 | x <- xs, p x ]

