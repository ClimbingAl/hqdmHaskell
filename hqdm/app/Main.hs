{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import HqdmLib (HqdmInput, RelationPair, Relation, getSubjects, getPredicates, uniqueIds, stringListSort, lookupHqdmOne, lookupHqdmType, lookupSubtypes, lookupSubtypeOf, lookupSubtypesOf, lookupSupertypeOf, lookupSupertypesOf, findSupertypeTree, printableTypeTree, findSubtypeTree, findInheritedRels, collapseInheritedRels)
import HqdmInspection (howmanyNodes)

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
-- import Data.List (sortUniq)
import qualified Data.Vector as V
import Data.String (String)
-- import Data.Tree (Tree(subForest))

-- Constants
hqdmInputFilename::String
hqdmInputFilename = "hqdmAllAsDataNoClassName.csv"

thing::String
thing = "hqdm:e5ec5d9e-afea-44f7-93c9-699cd5072d90"

event::String
event = "hqdm:545b4541-8a34-46b8-8704-2265be0244c3"

functionalObject::String
functionalObject = "hqdm:130e95f1-ebc4-46f1-90ba-3f9fa21cb77b"

classOfSpatiotemporalextent::String
classOfSpatiotemporalextent = "hqdm:bb6f6d3f-1ed1-41ab-942c-6b3667c5da37"

stateOfPhysicalObject::String
stateOfPhysicalObject = "hqdm:f9cb048d-a2f7-4ff6-b824-c59b44e2aabe"

requirement::String
requirement = "hqdm:46b71552-11c9-4be1-a118-c879db176c00"

main :: IO ()
main = do
    --file_contents <- readFile "hqdmAllAsDataNoParentheses.stmt"
     --putStrLn file_contents


    hqdmTriples <- fmap V.toList . decode @HqdmInput NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = either (const []) id hqdmTriples
    --print hqdmInputModel
    putStr "\n\nLoaded Data\n\n"

    let hqdmRawNodes = getSubjects hqdmInputModel
    
    -- Generate list of all hqdm:iris    
    let uniqueNodes = uniqueIds hqdmRawNodes

    --putStr "\n\nList of ids:\n\n"
    --print uniqueNodes

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

    putStr "\nGet the supertypes ids of given [things]:\n\n"
    let thingSupertypes = lookupSupertypesOf [classOfSpatiotemporalextent, event] subtypes 
    print (concat thingSupertypes)

    putStr "\nGet the supertypes of THING (should be []):\n\n"
    let thingSupertype = findHqdmTypesInList $ lookupSupertypeOf thing subtypes 
    print (thingSupertype)

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
    putStr "\nSupertype tree of stateOfPhysicalObject is:\n\n"
    let stTree = findSupertypeTree [[stateOfPhysicalObject]] subtypes
    print (stTree)

    putStr "\nPrintable Supertype tree of stateOfPhysicalObject is:\n\n"
    let printableStTree = printableTypeTree (reverse stTree) hqdmInputModel ""
    putStr (printableStTree)

    -- Find the subtypes all the way to the lowest accessible node by recursion
    putStr "\nSubtype tree of hqdm:stateOfPhysicalObject is:\n\n"
    let sbtTree = findSubtypeTree [[stateOfPhysicalObject]] subtypes []
    print (sbtTree)

    putStr "\nPrintable Type tree of stateOfPhysicalObject is:\n\n"
    let printableSbtTree = printableTypeTree sbtTree hqdmInputModel ""
    putStr (printableSbtTree)

    -- Take the result and compose a list of the relations inherited down the tree, via all paths
    ---- Take each hqdm type (by node id), calculate its relations (predicate and object (object aka. range)) and write out in EXPRESS-like format
    putStr "\nRelation pairs in supertype tree:\n\n"
    let stRels = findInheritedRels (concat stTree) hqdmInputModel []
    print stRels
    
    ---- Allow summary that just has predicates collpsed without ranges
    putStr "\nCollapse relation pairs to unique predicate names:\n\n"
    let collapsedStRels = collapseInheritedRels stRels
    print collapsedStRels

    -- Compare with HDQM triples


    -- Add Cardinalities as type patterns
    
    -- Compose the structural validations of hqdm

    -- 

    
    putStr "\n\nDone\n"
    
