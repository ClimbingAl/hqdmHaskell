{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmRelations Main
-- Description :  Construction and management of Relation Sets
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that generates the relation SETs for HQDM AllAsData.

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
    convertRelationByDomainRangeAndName,
    headListIfPresent
    )

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
    printableRelationPairs,
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
hqdmRelationsInputFilename = "./input/allHqdmRels.csv" -- allHqdmRels or exportedPureBinaryRelationsModded2

hqdmInputFilename::String
hqdmInputFilename = "./input/hqdmAllAsDataFormal2_AllRels.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1 or hqdmAllAsDataFormal2_AllRels

exampleBrelId::String
exampleBrelId = "hqdmRelation:f7c8c901-6147-4a58-bf88-0fdd32dc9faa" -- hqdm:individual hqdmel:member_of hqdm:class_of_individual

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
    
    putStrLn "Start, construct relations from ./input/exportedPureBinaryRelationsModded.csv"

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename

    let relationsInputModel =  csvRelationsToPure $ either (const []) id hqdmRelationSets
    -- print relationsInputModel

    putStr "\nLoaded Relation SET Data\n"

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
    --putStr ( printablePathFromTuples $ relIdNameTuples superBRPathToUniversal relationsInputModel)

    -- Re-calculate the missing inherited relations & export
    let allStRels = allSupertypeRels hqdmInputModel relationsInputModel
    -- print allStRels

    let zippedRelList = zip allStRels relationsInputModel
    let completedRels = completeSuperBRels zippedRelList 
    print completedRels

    -- Correct inherited cardinalities

    -- Correct hqdm:type relations (perhaps with exception in findSuperBinaryRelation')

    -- Convert the hqdmInputModel to use relationIds
    let allIdTriples = fmap (`convertRelationByDomainRangeAndName` completedRels) hqdmInputModel
    putStr "\nExport All Id Triples:\n\n"
    --print allIdTriples

    putStr "END"
