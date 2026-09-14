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
    HqdmBinaryRelationSet,
    HqdmBinaryRelationPure(..),
    universalRelationSet,
    getRelationNameFromRels,
    getPureDomain,
    getPureRelationId,
    getPureRelationName,
    getPureRange,
    getPureSuperRelation,
    getPureCardinalityMin,
    getPureCardinalityMax,
    getPureRedeclared,
    getBrelDomainFromRels,
    findBrelDomainSupertypes,
    findBrelFromId,
    superRelationPathsToUniversalRelation,
    relIdNameTuples,
    printablePathFromTuples,
    isSubtype,
    subtypesOfFilter,
    sortOnUuid,
    findBrelsWithDomains,
    findBrelsAndNamesWithDomains,
    findSuperBinaryRelation,
    findSuperBinaryRelation',
    printablePureRelation,
    csvRelationsFromPure,
    headListIfPresent,
    addNewCardinalitiesToPure,
    findMaxMaxCardinality,
    findMaxMinCardinality,
    )

import HqdmLib (
    Id,
    HqdmTriple,
    RelationPair,
    headIfUUIDPresent,
    getSubjects,
    getPredicates,
    uniqueIds,
    lookupHqdmOne,
    lookupHqdmType,
    lookupHqdmIdsFromTypePredicates,
    lookupSubtypes,
    lookupSubtypeOf,
    lookupSubtypesOf,
    lookupSupertypeOf,
    lookupSupertypesOf,
    findSupertypeTree,
    printableTypeTree,
    findSubtypeTree,
    findInheritedRels,
    collapseInheritedRels,
    printableRelationPairs)

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava

import Data.Csv (HasHeader( NoHeader ), decode, FromField(..), parseField)
import qualified Data.Vector as V
import Data.Either ( fromRight )
import Data.UUID (UUID, toString, fromString)
import Data.Maybe (fromJust)


-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../HqdmBinaryRelations_v7.csv"

hqdmInputFilename::String
hqdmInputFilename = "../HqdmTypes_v5Mapped.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1 or hqdmAllAsDataFormal4_AllRels

exampleBrelId::UUID
exampleBrelId = fromJust $ fromString "c037270e-801f-4957-ad79-239954cedc37" -- individual hqdm:member_of class_of_individual

allSupertypeRels:: [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [Maybe (RelationId, String)]
allSupertypeRels hqdmTriples pureBrels = fmap (\ x -> findSuperBinaryRelation' (getPureRelationId x) hqdmTriples pureBrels) pureBrels

main :: IO ()
main = do
    putStrLn ("Start, construct relations from " ++ hqdmRelationsInputFilename)

    csvData <- BL.readFile hqdmRelationsInputFilename

    -- Decode returns an Either String (V.Vector HqdmTriple)
    let decodeResult = decode @HqdmBinaryRelationPure NoHeader csvData

    case decodeResult of
        Left err -> do
            putStrLn "❌ CSV Parsing Failed!"
            putStrLn err  -- This will print the exact failure reason

        Right vectorModel -> do
            let hqdmRelationsModel = V.toList vectorModel
            putStr "\n\nLoaded Data Successfully\n\n"

            let relationsIds = map pureBinaryRelationId hqdmRelationsModel
            let uniqueNodes = HqdmLib.uniqueIds relationsIds

            putStr "Number of relation ids is:\n\n"
            print (length uniqueNodes)

    hqdmTriples <- fmap V.toList . decode @HqdmLib.HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = fromRight [] hqdmTriples

    putStr "\n\nLoaded HqdmAllAsData\n\n"

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelationPure NoHeader <$> BL.readFile hqdmRelationsInputFilename

    let pureHqdmRelations = fromRight [] hqdmRelationSets
    -- print relationsInputModel

    putStr "\n\nLoaded Relation SET Data\n\n"

    -- Compute relation supersets?? Leave the rigorous version of this for now. 

    let domainOfRel = getBrelDomainFromRels exampleBrelId pureHqdmRelations
    let nameOfRel = ( exampleBrelId, getRelationNameFromRels exampleBrelId pureHqdmRelations)
    putStr "\n\nName and then Domain of a particular Relation:\n\n"
    print nameOfRel
    print domainOfRel

    let subtypes = lookupSubtypes hqdmInputModel
    let domainSupertypesOfRel = findBrelDomainSupertypes exampleBrelId pureHqdmRelations subtypes
    putStr "\n\nDomain Supertypes of a particular Relation:\n\n"
    print domainSupertypesOfRel

    -- Find the likely superBR-set by finding supertype(s) until a match is found.
    let namesOfBrelsOfDomain = findBrelsAndNamesWithDomains domainSupertypesOfRel pureHqdmRelations
    putStr "\n\nIds and Names of supertype relations:\n\n"
    print namesOfBrelsOfDomain

    --let closestNameMatches = [x | x <- namesOfBrelsOfDomain, snd x `isPrefixOf` getRelationNameFromRels exampleBrelId pureHqdmRelations]
    {-putStr "\n\nClosest relations:\n\n"
    print closestNameMatches-}

    let supertypeBinaryRel = findSuperBinaryRelation' exampleBrelId hqdmInputModel pureHqdmRelations
    putStr "\n\nAll wrapped up in findSuperBinaryRelation' function (returns a Maybe):\n\n"
    print supertypeBinaryRel

    -- Now find the supertype relation for all the relations
    {-let allStRels = allSupertypeRels hqdmInputModel pureHqdmRelations--fmap (\ x -> findSuperBinaryRelation' (getPureRelationId x) hqdmInputModel pureHqdmRelations)
    let zippedRels = zip pureHqdmRelations allStRels
    let addedStRelsPure = fmap (\ x -> addStRelationToPure (snd x) (fst x)) zippedRels
    putStr "\n\nAll super-relations:\n\n"
    print addedStRelsPure-}

    --let printableStRels = csvRelationsFromPure addedStRelsPure
    --putStr "\n\nPrintable binary relations:\n\n"
    --putStr printableStRels

    putStr "\n\nRelations All Done!\n\n"