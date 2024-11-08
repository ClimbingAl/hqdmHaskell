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
    findBrelsWithDomains,
    findBrelsAndNamesWithDomains,
    findSuperBinaryRelation,
    findSuperBinaryRelation',
    addStRelationToPure,
    printablePureRelation,
    csvRelationsFromPure,
    lookupSuperBinaryRelOf
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
    printableRelationPairs)

import HqdmInspection (howmanyNodes)
import HqdmIds

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.List (isPrefixOf)

-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "./input/relationsWithPrefix_TypedIds2.csv"

hqdmInputFilename::String
hqdmInputFilename = "../hqdm/hqdmAllAsDataFormal1_NoExtensions.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1 or hqdmAllAsDataFormal2_AllRels

exampleBrelId::String
exampleBrelId = "hqdmRelation:c037270e-801f-4957-ad79-239954cedc37" -- hqdm:individual hqdmel:member_of hqdm:class_of_individual

allSupertypeRels:: [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [Maybe (RelationId, String)]
allSupertypeRels hqdmTriples pureBrels = fmap (\ x -> findSuperBinaryRelation' (getPureRelationId x) hqdmTriples pureBrels) pureBrels

main :: IO ()
main = do
    putStrLn "Start, construct relations from relations_xxx.csv"

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename

    let relationsInputModel = either (const []) id hqdmRelationSets
    -- print relationsInputModel

    putStr "\n\nLoaded Relation SET Data\n\n"

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = either (const []) id hqdmTriples
    --print hqdmInputModel
    putStr "\n\nLoaded HqdmAllAsData\n\n"

    -- Convert hqdmRelationInputSets to use ids from hqdmAllAsData instead of names
    putStr "\n\nCalc pure Hqdm Relations!\n\n"
    let pureHqdmRelations = hqdmRelationsToPure relationsInputModel hqdmInputModel
    -- print pureHqdmRelations

    -- Compute relation supersets?? Leave the rigorous version of this for now. 

    let domainOfRel = getBrelDomainFromRels exampleBrelId pureHqdmRelations
    let nameOfRel = ( exampleBrelId, getRelationNameFromRels exampleBrelId pureHqdmRelations)
    {-putStr "\n\nName and then Domain of a particular Relation:\n\n"
    print nameOfRel
    print domainOfRel-}

    let subtypes = lookupSubtypes hqdmInputModel
    let domainSupertypesOfRel = findBrelDomainSupertypes exampleBrelId pureHqdmRelations subtypes
    {-putStr "\n\nDomain Supertypes of a particular Relation:\n\n"
    print domainSupertypesOfRel-}

    -- Find the likely superBR-set by finding supertype(s) until a match is found.
    let namesOfBrelsOfDomain = findBrelsAndNamesWithDomains domainSupertypesOfRel pureHqdmRelations
    {-putStr "\n\nIds and Names of supertype relations:\n\n"
    print namesOfBrelsOfDomain-}

    let closestNameMatches = [x | x <- namesOfBrelsOfDomain, snd x `isPrefixOf` getRelationNameFromRels exampleBrelId pureHqdmRelations]
    {-putStr "\n\nClosest relations:\n\n"
    print closestNameMatches-}
    
    let supertypeBinaryRel = findSuperBinaryRelation' exampleBrelId hqdmInputModel pureHqdmRelations
    {-putStr "\n\nAll wrapped up in findSuperBinaryRelation' function (returns a Maybe):\n\n"
    print supertypeBinaryRel-}

    -- Now find the supertype relation for all the relations
    let allStRels = allSupertypeRels hqdmInputModel pureHqdmRelations--fmap (\ x -> findSuperBinaryRelation' (getPureRelationId x) hqdmInputModel pureHqdmRelations)
    let zippedRels = zip pureHqdmRelations allStRels
    let addedStRelsPure = fmap (\ x -> addStRelationToPure (snd x) (fst x)) zippedRels
    {-putStr "\n\nAll super-relations:\n\n"
    print addedStRelsPure-}

    let printableStRels = csvRelationsFromPure addedStRelsPure
    putStr "\n\nPrintable binary relations:\n\n"
    putStr printableStRels

    putStr "\n\nRelations All Done!\n\n"