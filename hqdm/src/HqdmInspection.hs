
module HqdmInspection
( 
    howmanyNodes
) where

import HqdmLib (HqdmTriple, subject )

-- getIndexOfThing :: [HqdmTriple] -> Int
-- getIndexOfThing x = findIndex ((subject "https://hqdmtop.github.io/hqdm#e5ec5d9e-afea-44f7-93c9-699cd5072d90") `elem`)

howmanyNodes :: (String -> Bool) -> [String] -> Int
howmanyNodes p xs = sum [ 1 | x <- xs, p x ]

