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
--
-- HQDM AllAsData Triples are now handled using the Haskell Data.UUID data type.


module Main (main) where

import HqdmRelations (
    RelationId,
    HqdmBinaryRelationPure(..),
    getRelationNameFromRels,
    getPureRelationId,
    getBrelDomainFromRels,
    findBrelDomainSupertypes,
    findBrelsAndNamesWithDomains,
    findSuperBinaryRelation',
    )

import HqdmLib (
    HqdmTriple,
    uniqueIds,
    lookupSubtypes)

import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.Either ( fromRight )
import Data.UUID (UUID, fromString)
import Data.Maybe (fromJust)


-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../HqdmBinaryRelations_v7.csv"

hqdmInputFilename::String
hqdmInputFilename = "../HqdmTypes_v5Mapped.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1 or hqdmAllAsDataFormal4_AllRels

exampleBrelId::UUID
exampleBrelId = fromJust $ fromString "c037270e-801f-4957-ad79-239954cedc37" -- individual hqdm:member_of class_of_individual

allSupertypeRels:: [HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [Maybe (HqdmRelations.RelationId, String)]
allSupertypeRels hqdmTriples pureBrels = fmap (\ x -> HqdmRelations.findSuperBinaryRelation' (HqdmRelations.getPureRelationId x) hqdmTriples pureBrels) pureBrels

main :: IO ()
main = do
    putStrLn ("Start, construct relations from " ++ hqdmRelationsInputFilename)

    csvData <- BL.readFile hqdmRelationsInputFilename

    -- Decode returns an Either String (V.Vector HqdmTriple)
    let decodeResult = decode @HqdmRelations.HqdmBinaryRelationPure NoHeader csvData

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

    hqdmRelationSets <- fmap V.toList . decode @HqdmRelations.HqdmBinaryRelationPure NoHeader <$> BL.readFile hqdmRelationsInputFilename

    let pureHqdmRelations = fromRight [] hqdmRelationSets
    -- print relationsInputModel

    putStr "\n\nLoaded Relation SET Data\n\n"

    -- Compute relation supersets?? Leave the rigorous version of this for now. 

    let domainOfRel = HqdmRelations.getBrelDomainFromRels exampleBrelId pureHqdmRelations
    let nameOfRel = ( exampleBrelId, HqdmRelations.getRelationNameFromRels exampleBrelId pureHqdmRelations)
    putStr "\n\nName and then Domain of a particular Relation:\n\n"
    print nameOfRel
    print domainOfRel

    let subtypes = HqdmLib.lookupSubtypes hqdmInputModel
    let domainSupertypesOfRel = HqdmRelations.findBrelDomainSupertypes exampleBrelId pureHqdmRelations subtypes
    putStr "\n\nDomain Supertypes of a particular Relation:\n\n"
    print domainSupertypesOfRel

    -- Find the likely superBR-set by finding supertype(s) until a match is found.
    let namesOfBrelsOfDomain = HqdmRelations.findBrelsAndNamesWithDomains domainSupertypesOfRel pureHqdmRelations
    putStr "\n\nIds and Names of supertype relations:\n\n"
    print namesOfBrelsOfDomain

    --let closestNameMatches = [x | x <- namesOfBrelsOfDomain, snd x `isPrefixOf` getRelationNameFromRels exampleBrelId pureHqdmRelations]
    {-putStr "\n\nClosest relations:\n\n"
    print closestNameMatches-}

    let supertypeBinaryRel = HqdmRelations.findSuperBinaryRelation' exampleBrelId hqdmInputModel pureHqdmRelations
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