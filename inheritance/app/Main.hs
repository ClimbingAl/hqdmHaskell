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

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V

-- Constants
hqdmInputFilename::String
hqdmInputFilename = "../hqdm/hqdmAllAsDataFormal1.csv"

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

    -- Write namedTypesAndRels to the console
    putStr (concatMap (\ x -> "\n\n\nTYPE: " ++ head (fst x) ++ "\n\nRELATIONS: " ++ concat (snd x)) namedTypesAndRels)

    --putStr (concatMap ("\n\n\n" ++) printableAllInheritedRels)


