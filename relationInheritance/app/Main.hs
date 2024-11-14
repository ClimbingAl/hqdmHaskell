{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmRelationInheritance Main
-- Description :  Construction and management of all Inherited Relation Sets and their Cardinalities
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that generates the relation SETs for all HQDM AllAsData.

module Main (main) where

import HqdmRelations (
    RelationId,
    HqdmRelationSet,
    RelationPair,
    HqdmBinaryRelation,
    HqdmBinaryRelationSet,
    HqdmBinaryRelationPure,
    universalRelationSet,
    getRelationNameFromRels,
    hqdmRelationsToPure,
    csvRelationsToPure,
    getPureDomain,
    getPureRelationId,
    getPureRelationName,
    getPureRange,
    getPureSuperRelation,
    getPureCardinalityMin,
    getPureCardinalityMax,
    getPureReclared,
    getPureRedeclaredFromRange,
    getBrelDomainFromRels,
    findBrelDomainSupertypes,
    findBrelFromId,
    superRelationPathToUniversalRelation,
    relIdNameTuples,
    printablePathFromTuples,
    findBrelsWithDomains,
    findBrelsAndNamesWithDomains,
    findSuperBinaryRelation,
    findSuperBinaryRelation',
    addStRelationToPure,
    printablePureRelation,
    csvRelationsFromPure,
    lookupSuperBinaryRelOf,
    hqdmSwapRelationNamesForIds,
    convertRelationByDomainAndRange,
    headListIfPresent,
    addNewCardinalitiesToPure,
    correctCardinalities,
    correctAllCardinalities,
    findMaxMaxCardinality,
    findMaxMinCardinality
    )

import HqdmLib (
    Id,
    HqdmTriple(..),
    HqdmTriple(subject, predicate, object),
    RelationPair,
    Relation,
    HqdmHasSupertype,
    getSubjects,
    getPredicates,
    uniqueIds,
    uniqueTriples,
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
    printableCollapsedList,
    printableRelationPairs,
    exportAsTriples,
    csvTriplesFromHqdmTriples,
    screenCharOffset,
    fmtString)

import HqdmInspection (howmanyNodes)
import HqdmIds

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.List (isPrefixOf)
import Data.Maybe

-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "./input/PureHqdmRelations_v2.csv" -- allHqdmRels or exportedPureBinaryRelationsModded2 or PureHqdmRelations_v0

hqdmInputFilename::String
hqdmInputFilename = "./input/HqdmAllAsDataFormal2.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1 or hqdmAllAsDataFormal2

exampleBrelId::String
exampleBrelId = "hqdmRelation:f7c8c901-6147-4a58-bf88-0fdd32dc9faa" -- hqdm:individual hqdm:temporal_part_of hqdm:class_of_individual

maybePrint :: Show a => Maybe a -> IO ()
maybePrint (Just x) = print x
maybePrint x        = print x

findStRelIfNotPresent:: HqdmBinaryRelationPure -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> Maybe (RelationId, String)
findStRelIfNotPresent rel hqdmTriples pureBrels
    | getPureSuperRelation rel /= "" = Just (getPureSuperRelation rel, getPureRelationName rel)
    | otherwise = findSuperBinaryRelation' (getPureRelationId rel) hqdmTriples pureBrels

allSupertypeRels:: [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [Maybe (RelationId, String)]
allSupertypeRels hqdmTriples pureBrels = fmap (\ x -> findStRelIfNotPresent x hqdmTriples pureBrels) pureBrels

completeSuperBRel:: (Maybe (RelationId, String), HqdmBinaryRelationPure) -> HqdmBinaryRelationPure
completeSuperBRel zippedRel
    | isNothing (fst zippedRel) = snd zippedRel
    | otherwise = uncurry addStRelationToPure zippedRel

completeSuperBRels:: [(Maybe (RelationId, String), HqdmBinaryRelationPure)] -> [HqdmBinaryRelationPure]
completeSuperBRels = fmap completeSuperBRel

main :: IO ()
main = do
    
    putStrLn ("Start, construct relations from " ++ hqdmRelationsInputFilename)

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename

    let relationsInputModel =  csvRelationsToPure $ either (const []) id hqdmRelationSets
    -- print relationsInputModel

    putStr ("\nLoaded Relation SET Data from " ++ hqdmInputFilename)

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = either (const []) id hqdmTriples
    --print hqdmInputModel
    putStr "\nLoaded HqdmAllAsData\n\n"

    -- Now replicate the inheritance queries but over the pure Relations
    
    -- Find the subtypes in the input model
    let subtypes = lookupSubtypes hqdmInputModel

    putStr "\nGet the superRelation id of given Binary Relation id:\n\n"
    let exampleSuperBR = lookupSuperBinaryRelOf exampleBrelId relationsInputModel
    --maybePrint exampleSuperBR

    let superBRPathToUniversal = superRelationPathToUniversalRelation exampleBrelId relationsInputModel []
    putStr ( printablePathFromTuples $ relIdNameTuples superBRPathToUniversal relationsInputModel)

    -- Re-calculate the missing inherited relations & export
    let allStRels = allSupertypeRels hqdmInputModel relationsInputModel
    -- print allStRels

    let zippedRelList = zip allStRels relationsInputModel
    let completedRels = completeSuperBRels zippedRelList 
    -- print completedRels

    let maxC = findMaxMinCardinality superBRPathToUniversal completedRels (-1)
    --print (maxC)

    let correctedExample = correctCardinalities (head $ findBrelFromId exampleBrelId completedRels) completedRels
    --print correctedExample

    let supeerBRPaths = fmap (\ x -> superRelationPathToUniversalRelation (getPureRelationId x) completedRels []) completedRels
    --print supeerBRPaths

    -- Correct inherited cardinalities
    -- for each BR, from the bottom-up, find the superBRPathToUniversal, go down each path - if the next item down the path has a less restricted cardinality than the current one then replace it with the current one
    -- Also add check that the superBR is always the universal one?
    let improvedCardinalities =  correctAllCardinalities completedRels
    let csvRelations = csvRelationsFromPure improvedCardinalities
    -- putStr csvRelations

    -- Convert the hqdmInputModel to use relationIds
    let allIdTriples =  hqdmSwapRelationNamesForIds hqdmInputModel completedRels 
    putStr "\nExport All Id Triples:\n\n"
    putStr (concat $ HqdmLib.csvTriplesFromHqdmTriples allIdTriples )

    putStr "END"
