{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import HqdmLib (HqdmInput, getSubjects, getPredicates, uniqueIds, stringListSort)
import HqdmInspection (howmanyNodes)

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
-- import Data.List (sortUniq)
import qualified Data.Vector as V

main :: IO ()
main = do
    --file_contents <- readFile "hqdmAllAsDataNoParentheses.stmt"
     --putStrLn file_contents

    hqdmTriples <- fmap V.toList . decode @HqdmInput NoHeader <$> BL.readFile "hqdmAllAsDataNoClassName.csv"

    let hqdmInputModel = either (const []) id hqdmTriples
    print hqdmInputModel
    --putStr "\n\nLoaded Data\n\n"

    let hqdmRawNodes = getSubjects hqdmInputModel
    
    -- Generate list of all hqdm:iris    
    let uniqueNodes = uniqueIds hqdmRawNodes

    putStr "\n\nList of ids:\n\n"
    print uniqueNodes

    putStr "\nNumber of ids is:\n\n"
    print (length uniqueNodes)

    -- Generate List of Labelled Nodes


    ---------------------------------------------

    let hqdmRawPredicates = getPredicates hqdmInputModel
    let uniquePredicates = stringListSort ( uniqueIds hqdmRawPredicates )

    putStr "\n\nList of predicates:\n\n"
    print uniquePredicates

    putStr "\nNumber of predicates is:\n\n"
    print (length uniquePredicates)
  
    -- Get Top Level Thing

    putStr "\n\nHow many thing nodes:\n\n"
    let numThings = howmanyNodes (=="hqdm:e5ec5d9e-afea-44f7-93c9-699cd5072d90") uniqueNodes
    print numThings

    -- Compose the structural validations of hqdm
    
    putStr "\n\nDone\n"
    
