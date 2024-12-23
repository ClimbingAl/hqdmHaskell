{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmJoin Main
-- Description :  Join MagmaCore datasets with Pure Hqdm
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that joins a MagmaCore generated dataset with the prepared pure HQDM binary 
-- relation SETs and HQDM AllAsData.

module Main (main) where

import HqdmRelations (
    RelationId,
    HqdmRelationSet,
    RelationPair,
    HqdmBinaryRelation,
    HqdmBinaryRelationSet,
    HqdmBinaryRelationPure,
    CardinalityCheck (Valid, Invalid),
    RelationSetCheck,
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
    getPureRedeclared,
    getPureRedeclaredFromRange,
    printRelation,
    getBrelDomainFromRels,
    findBrelDomainSupertypes,
    findBrelFromId,
    findBrelsFromDomain,
    superRelationPathsToUniversalRelation,
    relIdNameTupleLayers,
    relIdNameTuples,
    printablePathFromTuples,
    isSubtype,
    subtypesOfFilter,
    sortOnUuid,
    findBrelsWithDomains,
    findBrelsAndNamesWithDomains,
    findSuperBinaryRelation,
    findSuperBinaryRelation',
    --addStRelationToPure,
    printablePureRelation,
    csvRelationsFromPure,
    lookupSuperBinaryRelsOf,
    hqdmSwapTopRelationNamesForIds,
    convertTopRelationByDomainAndName,
    headListIfPresent,
    addNewCardinalitiesToPure,
    correctCardinalities,
    correctAllCardinalities,
    findMaxMaxCardinality,
    findMaxMinCardinality,
    hqdmSwapAnyRelationNamesForIds,
    printableLayerWithDomainAndRange,
    printablePathFromTuplesWithDomainAndRange
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
    fmtString
    )

import HqdmQueries (
    part,
    set,
    order,
    emergent,
    filterRelsBy,
    filterRelsByPart,
    filterRelsBySet
    )

import HqdmInspection (howmanyNodes)
import HqdmIds

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.List (isPrefixOf, sortOn)
import Data.Maybe
import Data.Either

-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../PureHqdmRelations_v6.csv"

hqdmInputFilename::String
hqdmInputFilename = "../HqdmAllAsDataFormal3Short.csv"

joinModelFilename::String
joinModelFilename = "./input/networksBasic1converted.csv"

elementOfType::String
elementOfType = "8130458f-ae96-4ab3-89b9-21f06a2aac78"

hasSuperclass::String
hasSuperclass = "7d11b956-0014-43be-9a3e-f89e2b31ec4f"

main :: IO ()
main = do
    putStrLn ("Start HqdmJoin, load relations from " ++ hqdmRelationsInputFilename)

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets
    -- print relationsInputModel

    putStr ("\nLoaded Relation SET Data.  Now load HQDM types and their relations all as data from " ++ hqdmInputFilename)

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename

    let hqdmInputModel = fromRight [] hqdmTriples
    --print hqdmInputModel
    putStr "\n\nLoaded HqdmAllAsData\n\n"

    joinModelTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile joinModelFilename
    let joinInputModel = fromRight [] joinModelTriples
    putStr ("\n\nLoaded Model to Join: " ++ joinModelFilename)

    -- Get the unique node ids of the input model
    let uniqueJoinNodes = uniqueIds $ getSubjects joinInputModel
    putStr "\nNumber of ids is:\n\n"
    print (length uniqueJoinNodes)

    putStr "\nGet the entire object and find its type (assumes ordered list of types):\n\n"
    let nodeTypeStatements = fmap (\ x -> head $ lookupHqdmOne x joinInputModel) uniqueJoinNodes
    --print nodeTypeStatements

    -- Get the hqdmInputModel node ids of the types that are in the join Model
    let subtypes = lookupSubtypes hqdmInputModel

    let typeIdsOfJoinObjects = zip uniqueJoinNodes (fmap (head . lookupHqdmIdFromType hqdmInputModel . object)  nodeTypeStatements)
    -- Now filter the objects to join to be only those that are subtypes of ste
    let onlySubtypesOfSte = subtypesOfFilter typeIdsOfJoinObjects spatio_temporal_extent subtypes
    --print(isSubtype (snd $ head typeIdsOfJoinObjects) spatio_temporal_extent subtypes )

    let elementOfTypeName = getRelationNameFromRels elementOfType relationsInputModel
    let elementOfTypeTriples = fmap (\ x -> HqdmTriple (fst x) elementOfTypeName (snd x)) onlySubtypesOfSte

    putStr "\nNew element_of_type triples\n\n"
    print elementOfTypeTriples

    putStr "\nNumber of element_of_type predicates is:\n\n"
    print (length elementOfTypeTriples)

    let onlySubtypesOfClass = subtypesOfFilter typeIdsOfJoinObjects hqdmClass subtypes
    let hasSuperClassName = getRelationNameFromRels hasSuperclass relationsInputModel
    let hasSuperclassTriples = fmap (\ x -> HqdmTriple (fst x) hasSuperClassName (snd x)) onlySubtypesOfClass

    putStr "\nNew class triples\n\n"
    print hasSuperclassTriples

    putStr "\nNumber of new class predicates is:\n\n"
    print (length hasSuperclassTriples)

    -- Join the triples
    putStr "\nDo resulting list lengths sum to the input length?\n\n"
    print ((length hasSuperclassTriples + length elementOfTypeTriples) == length uniqueJoinNodes)

    --putStr "\nNow do the join and show the results:\n\n"

    let joinedResults = sortOnUuid $ joinInputModel ++ hasSuperclassTriples ++ elementOfTypeTriples
    --putStr (concat $ HqdmLib.csvTriplesFromHqdmTriples joinedResults)

    putStr "\nResults length:"
    print (length joinedResults)

    let allRelationIdJoinedTriples =  sortOnUuid $ hqdmSwapAnyRelationNamesForIds joinedResults hqdmInputModel relationsInputModel

    --putStr "\nExport the joined model all with predicates as Relation Ids:\n\n"
    --putStr (concat $ HqdmLib.csvTriplesFromHqdmTriples allRelationIdJoinedTriples )

    let exampleObjectId = "7e181b8d-0aed-46ee-928e-b08d60d0ed58"
    let exampleObjectTriples = lookupHqdmOne exampleObjectId allRelationIdJoinedTriples

    -- Type
    let exampleObjectType =  head $ lookupHqdmIdFromType hqdmInputModel (head $ lookupHqdmType exampleObjectTriples ) 
    putStr ("\n\nExample object type id: " ++ exampleObjectType ++ "\n\n")

    -- What BR sets apply to this type?
    let exampleObjectTypeBRelSets = findBrelsFromDomain exampleObjectType relationsInputModel
    print exampleObjectTypeBRelSets

    -- Query for set membership of an element
    putStr "\n\nSet membership predicates:\n\n"
    let setMemberships = filterRelsBySet exampleObjectTriples relationsInputModel
    print setMemberships

    -- Query for members of a set   

    -- Query for part-hood relations of an element
    putStr "\n\nPart predicates:\n\n"
    let partPredicates = filterRelsByPart exampleObjectTriples relationsInputModel
    print partPredicates

    --- Now find things that are part of it... and then trasitively


    --- Check for Cardiality violations
    -- Fmap each binary relation set, count number of instances of relations matching each set (and their range?), compare count with max and min cardinality - mark success or fail.
    --let testResult = RelationSetCheck Valid (head exampleObjectTypeBRelSets)
    --print testResult

    putStr "\n\nDONE\n\n"
