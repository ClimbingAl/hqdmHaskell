
module HqdmInspection
( 
    howmanyNodes
) where

howmanyNodes :: (String -> Bool) -> [String] -> Int
howmanyNodes p xs = sum [ 1 | x <- xs, p x ]

