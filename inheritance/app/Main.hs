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
    printableCollapsedList,
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

    let uniqueNodes = uniqueIds $ getSubjects hqdmInputModel

    -- Find the subtypes in the input model
    let subtypes = lookupSubtypes hqdmInputModel

    -- Find inherited relations for all the input hqdm nodes
    let allInheritedRels =  fmap (\ x -> findInheritedRels (concat (findSupertypeTree [[x]] subtypes)  ) hqdmInputModel []) uniqueNodes
    let printableAllInheritedRels = fmap (printableRelationPairs hqdmInputModel) allInheritedRels

    -- Make a collapsed version with the list of relations that the given type should have
    let allInheritedRelsCollapsed = fmap (printableCollapsedList . collapseInheritedRels) allInheritedRels
    let namedUniqueNodes = fmap (\ x -> take 1 (lookupHqdmType $ lookupHqdmOne x hqdmInputModel)) uniqueNodes
    let namedTypesAndRels = zip namedUniqueNodes allInheritedRelsCollapsed

    --print namedTypesAndRels

    -- Note: Possibly compare with HDQM triples

    -- Print all supertypes
    --let allTypeSupertypes = fmap (\ x ->  (x ++ "  " ++ (concat (findHqdmTypesInList (lookupSupertypeOf x subtypes) hqdmInputModel)))) uniqueNodes
    --print allTypeSupertypes

    putStr (concatMap (\ x -> "\n\n\nTYPE: " ++ head (fst x) ++ "\n\nRELATIONS: " ++ concat (snd x)) namedTypesAndRels)

    --putStr (concatMap ("\n\n\n" ++) printableAllInheritedRels)


