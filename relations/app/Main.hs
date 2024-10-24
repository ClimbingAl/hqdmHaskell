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
    HqdmBinaryRelation,
    RelationId,
    hqdmRelationsToPure)

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

-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "./input/relationsWithPrefix_TypedIds2.csv"

hqdmInputFilename::String
hqdmInputFilename = "../hqdm/hqdmAllAsDataFormal1_NoExtensions.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1


main :: IO ()
main = do
    putStrLn "Start, construct relations from relations_xxx.csv"

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename

    let relationsInputModel = either (const []) id hqdmRelationSets
    print relationsInputModel

    putStr "\n\nLoaded Relation SET Data\n\n"

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = either (const []) id hqdmTriples
    --print hqdmInputModel
    putStr "\n\nLoaded HqdmAllAsData\n\n"

    let hqdmRawNodes = getSubjects hqdmInputModel

    -- Generate list of all hqdm:iris    
    let uniqueNodes = uniqueIds hqdmRawNodes

    -- Convert hqdmRelationInputSets to use ids from hqdmAllAsData instead of names
    putStr "\n\nCalc pure Hqdm Relations!\n\n"
    let pureHqdmRelations = hqdmRelationsToPure relationsInputModel hqdmInputModel
    print pureHqdmRelations

    -- Compute relation supersets?? Leave the rigorous version of this for now. 

    -- Find the likely superBR-set by finding supertype(s) until a match is found.
    
    
    
    putStr "\n\nRelations All Done!\n\n"