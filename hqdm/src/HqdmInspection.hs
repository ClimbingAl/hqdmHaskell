
module HqdmInspection
( 
    howmanyNodes
) where

import Data.UUID (UUID)

howmanyNodes :: (UUID -> Bool) -> [UUID] -> Int
howmanyNodes p xs = sum [ 1 | x <- xs, p x ]

