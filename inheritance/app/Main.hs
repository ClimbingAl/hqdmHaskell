{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import HqdmLib (
    Id,
    HqdmInput,
    RelationPair,
    Relation,
    HqdmHasSupertype,
    getSubjects,
    getPredicates,
    uniqueIds,
    stringListSort,
    lookupHqdmOne,
    lookupHqdmType,
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

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.String (String)

-- Constants
hqdmInputFilename::String
hqdmInputFilename = "../hqdm/hqdmAllAsDataNoClassName.csv"

thing::String
thing = "hqdm:e5ec5d9e-afea-44f7-93c9-699cd5072d90"

hqdmClass::String
hqdmClass = "hqdm:4a8cba08-035c-4902-935b-26da61ed282c"

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

-- Lookup inherited rels for each node in a list of Node Ids
--lookupInheritedRels :: [Id] -> [HqdmHasSupertype] -> [HqdmInput] -> [[[RelationPair]]]
--lookupInheritedRels ids = 

main :: IO ()
main = do

    hqdmTriples <- fmap V.toList . decode @HqdmInput NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = either (const []) id hqdmTriples

    let hqdmRawNodes = getSubjects hqdmInputModel

    let uniqueNodes = uniqueIds hqdmRawNodes

    -- Exercise functions to find the thing object's relations (triples) and then the type predicate for thing
    let thingObj = lookupHqdmOne thing hqdmInputModel
    let thingType = lookupHqdmType thingObj

    -- Find the subtypes of a given thing
    let subtypes = lookupSubtypes hqdmInputModel
    let thingSubtypes = findHqdmTypesInList (lookupSubtypeOf classOfSpatiotemporalextent subtypes) hqdmInputModel

    -- Find the supertypes of a given thing
    let thingSupertypes = findHqdmTypesInList (lookupSupertypeOf classOfSpatiotemporalextent subtypes) hqdmInputModel
    let thingSupertypes = lookupSupertypesOf [classOfSpatiotemporalextent, event] subtypes
    let thingSupertype = findHqdmTypesInList (lookupSupertypeOf thing subtypes) hqdmInputModel

    ---------------------------------------------

    let hqdmRawPredicates = getPredicates hqdmInputModel
    let uniquePredicates = stringListSort $ uniqueIds hqdmRawPredicates

    ---------------------------------------------
    -- Find the supertypes all the way to thing by recursion
    let stTree = findSupertypeTree [[thing]] subtypes
    let printableStTree = printableTypeTree (reverse stTree) hqdmInputModel ""
    --putStr printableStTree

    -- Find the subtypes all the way to the lowest accessible node by recursion
    let sbtTree = findSubtypeTree [[stateOfPhysicalObject]] subtypes []
    let printableSbtTree = printableTypeTree sbtTree hqdmInputModel ""

    -- Take the result and compose a list of the relations inherited down the tree, via all paths
    ---- Take each hqdm type (by node id), calculate its relations (predicate and object (object aka. range)) and write out in EXPRESS-like format
    let stRels = findInheritedRels (concat stTree) hqdmInputModel []
    let printableStRels = printableRelationPairs hqdmInputModel (reverse stRels)

    ---- Allow summary that just has predicates collpsed without ranges
    let collapsedStRels = collapseInheritedRels stRels

    -- Do this for all HQDM Types
    let allInheritedRels =  fmap (\ x -> findInheritedRels (concat (findSupertypeTree [[x]] subtypes)  ) hqdmInputModel []) uniqueNodes
    --print allInheritedRels
    let printableAllInheritedRels = fmap (printableRelationPairs hqdmInputModel) allInheritedRels
    putStr (concatMap ("\n\n\n" ++) printableAllInheritedRels)

    -- Compare with HDQM triples

    -- Print all supertypes
    let allTypeSupertypes = fmap (\ x ->  (x ++ "  " ++ (concat (findHqdmTypesInList (lookupSupertypeOf x subtypes) hqdmInputModel)))) uniqueNodes
    --print allTypeSupertypes
    -- Add Cardinalities as type patterns

    -- Compose the structural validations of hqdm

    -- 


    putStr "\n\nDone\n"


