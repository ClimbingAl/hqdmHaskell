{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmRelationInheritance Main
-- Description :  Checks of inheritance and cardinalities for all Inherited Relation Sets
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that generates the check results for all the BRels in hqdmRelationsInputFilename

module Main (main) where

import HqdmRelations (
    HqdmBinaryRelation,
    correctAllCardinalities,
    csvRelationsToPure,
    getPureRelationId,
    printablePathFromTuplesWithDomainAndRange,
    relIdNameTupleLayers,
    superRelationPathsToUniversalRelation
    )

import HqdmLib (
    HqdmTriple(..)
    )

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.Either


-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../PureHqdmRelations_v8.csv" -- allHqdmRels or exportedPureBinaryRelationsModded2 or PureHqdmRelations_v0 or ...

hqdmInputFilename::String
hqdmInputFilename = "../HqdmAllAsDataFormal4Short.csv"  -- hqdmAllAsDataFormal1_NoExtensions or hqdmAllAsDataFormal1 or hqdmAllAsDataFormal4 or ...

maybePrint :: Show a => Maybe a -> IO ()
maybePrint (Just x) = print x
maybePrint x        = print x

main :: IO ()
main = do

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename
    let hqdmInputModel = fromRight [] hqdmTriples

    -- Query the superRelationPath to the Universal Relation Set (Test will be False if this doesn't always resolve in a path to the Universal Relation Set)
    let superBRPaths = fmap (\ x -> superRelationPathsToUniversalRelation [[getPureRelationId x]] relationsInputModel ) relationsInputModel
    let testForPathTerminationAtUniversal = fmap (elem "85e78ac0-ec72-478f-9aac-cacb520290a0" . last) superBRPaths
    putStrLn ("\n\nDo all SuperBR paths terminate at the Universal BRel Set?  " ++ show (allTrue testForPathTerminationAtUniversal))

    -- Correct inherited cardinalities
    -- for each BR, from the bottom-up, find the superBRPathToUniversal, go down each path - if the next item down the path has a less restricted cardinality than the current one then replace it with the current one
    -- Also add check that the superBR is always the universal one?
    let improvedCardinalities =  correctAllCardinalities relationsInputModel
    let zippedBRels = zip relationsInputModel improvedCardinalities
    let testThatCardinalitiesMatch = fmap (\ x -> fst x == snd x) zippedBRels
    putStrLn ("\n\nDo all Cardinalities propagate down the BRel inheritance path?  " ++ show (allTrue testThatCardinalitiesMatch))
    --let csvRelations = csvRelationsFromPure improvedCardinalities
    --putStr csvRelations

    let printableSuperBRPaths = concatMap (\ x -> "\n\n\n\n" ++ printablePathFromTuplesWithDomainAndRange (relIdNameTupleLayers x relationsInputModel) relationsInputModel hqdmInputModel) superBRPaths
    putStr printableSuperBRPaths

    putStr "\n\nEND\n"

allTrue:: [Bool] -> Bool
allTrue = and