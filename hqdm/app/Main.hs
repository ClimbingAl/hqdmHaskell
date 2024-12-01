{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HQDM Main
-- Description :  Basic inspection of HQDM AllAsData Triples using HqdmLib functions
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that reads in HQDM AllAsData triples and parses them using the
-- HqdmLib functions.  Includes querying individual HQDM Entity Types by NodeId,
-- finding subtypes and supertypes of given NodeIds, subtype and supertype Trees
-- and computing the inheritance of relations given in HQDM AllAsData.

module Main (main) where

import HqdmLib (
    Id,
    HqdmTriple,
    RelationPair,
    Relation,
    getSubjects,
    getPredicates,
    uniqueIds,
    stringListSort,
    lookupHqdmOne,
    lookupHqdmType,
    lookupHqdmIdFromType,
    lookupSubtypes,
    lookupSubtypeOf,
    lookupSubtypesOf,
    lookupSupertypeOf,
    lookupSupertypesOf,
    findHqdmTypesInList,
    findSupertypeTree,
    printableTypeTree,
    findSubtypeTree,
    findInheritedRels,
    collapseInheritedRels,
    printableRelationPairs)
import HqdmInspection (howmanyNodes)
import HqdmIds

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
--import qualified Data.Text as Text (take, drop)

-- Constants
hqdmInputFilename::String
hqdmInputFilename = "hqdmAllAsDataFormal1_NoExtensions.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1

slice::Int -> Int -> String -> String
slice a b = take (b-a) . drop a

exportHqdmConstants :: [(Id, String)] -> String
exportHqdmConstants tpls = concatMap (\ x -> (snd x) ++ "::String\n" ++ (snd x) ++ " = \"" ++ (fst x) ++ "\"\n\n") tpls

main :: IO ()
main = do

    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = either (const []) id hqdmTriples
    --print hqdmInputModel
    putStr "\n\nLoaded Data\n\n"

    let hqdmRawNodes = getSubjects hqdmInputModel

    -- Generate list of all iris    
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
    let thingSubtypes = findHqdmTypesInList (lookupSubtypeOf class_of_spatio_temporal_extent subtypes) hqdmInputModel
    print thingSubtypes

    -- Find the supertypes of a given thing
    putStr "\nGet the names of supertypes of given thing:\n\n"
    let thingSupertypeNames = findHqdmTypesInList (lookupSupertypeOf class_of_spatio_temporal_extent subtypes) hqdmInputModel
    print thingSupertypeNames

    putStr "\nGet the supertypes ids of given [things]:\n\n"
    let thingSupertypes = lookupSupertypesOf [hqdmClass] subtypes
    print (concat thingSupertypes)

    putStr "\nGet the supertypes of THING (should be []):\n\n"
    let thingSupertype = findHqdmTypesInList (lookupSupertypeOf thing subtypes) hqdmInputModel
    print thingSupertype

    ----------------------------------------------

    let hqdmRawPredicates = getPredicates hqdmInputModel
    let uniquePredicates = stringListSort $ uniqueIds hqdmRawPredicates

    putStr "\n\nList of predicates:\n\n"
    print uniquePredicates

    putStr "\nNumber of predicates is:\n\n"
    print (length uniquePredicates)

    -- Get Top Level Thing

    putStr "\n\nHow many thing nodes (not a very clever test as it is really just testing if the the string is in the unique list):\n\n"
    let numThings = howmanyNodes (==thing) uniqueNodes
    print numThings

    ---------------------------------------------
    -- Find the supertypes all the way to thing by recursion
    putStr "\nSupertype tree of thing is:\n\n"
    let stTree = findSupertypeTree [[thing]] subtypes
    print stTree

    putStr "\nPrintable Supertype tree of thing is:\n\n"
    let printableStTree = printableTypeTree (reverse stTree) hqdmInputModel ""
    putStr printableStTree

    -- Find the subtypes all the way to the lowest accessible node by recursion
    putStr "\nSubtype tree of stateOfPhysicalObject is:\n\n"
    let sbtTree = findSubtypeTree [[state_of_physical_object]] subtypes []
    print sbtTree

    putStr "\nPrintable Type tree of stateOfPhysicalObject is:\n\n"
    let printableSbtTree = printableTypeTree sbtTree hqdmInputModel ""
    putStr printableSbtTree

    -- Take the result and compose a list of the relations inherited down the tree, via all paths
    ---- Take each hqdm type (by node id), calculate its relations (predicate and object (object aka. range)) and write out in EXPRESS-like format
    putStr "\nRelation pairs in supertype tree:\n\n"
    let stRels = findInheritedRels (concat stTree) hqdmInputModel []
    print stRels

    putStr "\nPrintable relation pairs in supertype tree:\n\n"
    let printableStRels = printableRelationPairs hqdmInputModel (reverse stRels)
    putStr printableStRels

    ---- Allow summary that just has predicates collpsed without ranges
    putStr "\nCollapse relation pairs to unique predicate names:\n\n"
    let collapsedStRels = collapseInheritedRels stRels
    print collapsedStRels

    -- Add Cardinalities as type patterns

    -- Generate Haskell constants from input data for HqdmIds Module
    -- Make list of pairs... [[UniqueNodes], [nodeTypeNames]]
    let namedUniqueNodes = fmap (\ x -> take 1 (lookupHqdmType $ lookupHqdmOne x hqdmInputModel)) uniqueNodes
    let unlistedNamedUniqueNodes = fmap concat namedUniqueNodes
    let slicedNamedNodes = fmap (\ x -> slice 5 (length x) x) unlistedNamedUniqueNodes
    let hqdmTypeIdsAndNames = zip uniqueNodes slicedNamedNodes
    let hqdmConstants = exportHqdmConstants hqdmTypeIdsAndNames

    -- Use this to output HQDM Constants for use in HqdmIds.hs.  Beware class::String.  Must change to something else like hqdmClass.
    -- putStr hqdmConstants

    -- Test the lookupHqdmIdFromType :: [HqdmTriple] -> String -> [Id] function
    let idOfThing = lookupHqdmIdFromType hqdmInputModel "thing"
    putStr "\n\nId of thing is:\n"
    print (head idOfThing)

    putStr "\n\nDone\n"

