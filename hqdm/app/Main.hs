{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import HqdmLib (HqdmInput, getSubjects, getPredicates, uniqueIds, stringListSort, lookupHqdmOne, lookupHqdmType, lookupSubtypes, lookupSubtypeOf, lookupSupertypeOf)
import HqdmInspection (howmanyNodes)

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
-- import Data.List (sortUniq)
import qualified Data.Vector as V
import Data.String (String)
-- import Data.Tree (Tree(subForest))

hqdmInputFilename = "hqdmAllAsDataNoClassName.csv"
thing = "hqdm:e5ec5d9e-afea-44f7-93c9-699cd5072d90"
classOfSpatiotemporalextent = "hqdm:bb6f6d3f-1ed1-41ab-942c-6b3667c5da37"

main :: IO ()
main = do
    --file_contents <- readFile "hqdmAllAsDataNoParentheses.stmt"
     --putStrLn file_contents


    hqdmTriples <- fmap V.toList . decode @HqdmInput NoHeader <$> BL.readFile hqdmInputFilename

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

    -- Exercise functions to find the thing object's relations (triples) and then the type predicate for thing
    putStr "\nGet the entire Thing object type:\n\n"
    let thingObj = lookupHqdmOne thing hqdmInputModel
    let thingType = lookupHqdmType thingObj
    print (head thingType)

    -- Find the subtypes of a given thing
    putStr "\nGet the subtypes of given thing:\n\n"
    let subtypes = lookupSubtypes hqdmInputModel
    -- Use constants above for thing or classOfSpatiotemporalextent
    let findHqdmTypesInList = fmap (\x -> head (lookupHqdmType $ lookupHqdmOne x hqdmInputModel))

    let thingSubtypes = findHqdmTypesInList $ lookupSubtypeOf classOfSpatiotemporalextent subtypes
    print (thingSubtypes)

    -- Find the supertypes of a given thing
    putStr "\nGet the supertypes of given thing:\n\n"
    let thingSupertypes = findHqdmTypesInList $ lookupSupertypeOf classOfSpatiotemporalextent subtypes 
    print (thingSupertypes)

    ---------------------------------------------

    let hqdmRawPredicates = getPredicates hqdmInputModel
    let uniquePredicates = stringListSort $ uniqueIds hqdmRawPredicates 

    putStr "\n\nList of predicates:\n\n"
    print uniquePredicates

    putStr "\nNumber of predicates is:\n\n"
    print (length uniquePredicates)
  
    -- Get Top Level Thing

    putStr "\n\nHow many thing nodes (not a very clever test as it is really just testing if the the string is in the unique list):\n\n"
    let numThings = howmanyNodes (==thing) uniqueNodes
    print (numThings)

    ---------------------------------------------
    -- Find the supertypes all the way to thing by recursion

    -- Take the result and compose a list of the relations inherited down the tree, via all paths

    -- Compare with HDQM triples

    -- Add Cardinalities as type patterns
    
    -- Compose the structural validations of hqdm

    -- 

    
    putStr "\n\nDone\n"
    
