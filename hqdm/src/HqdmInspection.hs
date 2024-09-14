{-# LANGUAGE DeriveGeneric #-}

module HqdmInspection
( 
    howmanyNodes
) where

import HqdmLib (HqdmInput, subject )

-- getIndexOfThing :: [HqdmInput] -> Int
-- getIndexOfThing x = findIndex ((subject "https://hqdmtop.github.io/hqdm#e5ec5d9e-afea-44f7-93c9-699cd5072d90") `elem`)

--nodeCount   :: Eq String => String -> [HqdmInput] -> Int
-- nodeCount x =  length . filter (== (subject x))

-- howmanyNodes :: (String -> Bool) -> [HqdmInput] -> Int 
-- howmanyNodes _ [ ] = 0
-- howmanyNodes a (x:xs)  = if a == (subject x) then 1 + howmanyNodes a xs 
--                                else howmanyNodes a xs

howmanyNodes p xs = sum [ 1 | x <- xs, p x ]


-- getNode :: String [HqdmInput] -> HqdmInput
-- getNode [] [] = []
-- getNode x [] = []
-- getNode [] y = []
-- getNode x y