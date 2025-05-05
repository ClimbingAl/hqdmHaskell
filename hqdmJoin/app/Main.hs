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
-- Executable Main used to develop the join of a MagmaCore generated dataset with the prepared pure HQDM binary 
-- relation SETs and HQDM AllAsData and parse with Binary Relation cardinality and range checks

module Main (main) where

import HqdmRelations (
    RelationId,
    HqdmRelationSet,
    RelationPair,
    HqdmBinaryRelation,
    HqdmBinaryRelationSet,
    HqdmBinaryRelationPure,
    RelationCheck (Valid, Invalid),
    relationSetCheck,
    relationSetAndIdCheck,
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
    printRelationWithTypeNames,
    getBrelDomainFromRels,
    findBrelDomainSupertypes,
    findBrelFromId,
    findBrelsFromIds,
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

    printablePureRelation,
    csvRelationsFromPure,
    lookupSuperBinaryRelsOf,
    hqdmSwapTopRelationNamesForIds,
    convertTopRelationByDomainAndName,
    convertAnyHqdmRelationByDomainRangeAndName,
    headListIfPresent,
    addNewCardinalitiesToPure,
    correctCardinalities,
    correctAllCardinalities,
    findMaxMaxCardinality,
    findMaxMinCardinality,
    hqdmSwapAnyRelationNamesForIds,
    hqdmSwapAnyRelationNamesForIdsStrict,
    printableLayerWithDomainAndRange,
    printablePathFromTuplesWithDomainAndRange, 
    lookupSubBRelsOf,
    validityFilter,
    cardinalityTestAllObjects,
    getTypeIdFromObject,
    cardinalityMetAllRels,
    printableErrorResults,
    filterOutErrorsBy,
    cardinalityMet,
    rangeTestAllObjects,
    rangeMetTest,
    findSubBRelTreeWithCount,
    filterHigherLevelBrels,
    relationInSupertypePaths
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
    lookupHqdmIdsFromTypePredicates,
    lookupSubtypes,
    lookupSubtypeOf,
    lookupSubtypesOf,
    lookupSupertypeOf,
    lookupSupertypesOf,
    findHqdmTypesInList,
    findHqdmTypeNamesInList,
    findHqdmNamesInList,
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
    filterRelsBySet,
    transitiveQueryFromLeft,
    transitiveQueryFromRight
    )

import HqdmIds
import HqdmMermaid

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.Either

-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../PureHqdmRelations_v91.csv"

hqdmInputFilename::String
hqdmInputFilename = "../HqdmAllAsDataFormal4Short.csv"

joinModelFilename::String
joinModelFilename = "./input/networksBasic1converted.csv"

elementOfType::String
elementOfType = "8130458f-ae96-4ab3-89b9-21f06a2aac78"

hasSuperclass::String
hasSuperclass = "7d11b956-0014-43be-9a3e-f89e2b31ec4f"

partOf::String
partOf = "be900942-8601-4254-9a12-d87a5bfa05d3"

main :: IO ()
main = do
    --putStrLn ("Start HqdmJoin, load relations from " ++ hqdmRelationsInputFilename)

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets
    -- print relationsInputModel

    --putStr ("\nLoaded Relation SET Data.  Now load HQDM types and their relations all as data from " ++ hqdmInputFilename)

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile hqdmInputFilename
    let hqdmInputModel = fromRight [] hqdmTriples
    --print hqdmInputModel
    --putStr "\n\nLoaded HqdmAllAsData\n\n"

    joinModelTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile joinModelFilename
    let joinInputModel = fromRight [] joinModelTriples
    --putStr ("\n\nLoaded Model to Join: " ++ joinModelFilename)

    -- Get the unique node ids of the input model
    let uniqueJoinNodes = uniqueIds $ getSubjects joinInputModel
    --putStr "\nNumber of ids is:\n\n"
    --print (length uniqueJoinNodes)

    --putStr "\nGet the entire object and find its type (assumes ordered list of types):\n\n"
    let nodeTypeStatements = fmap (\ x -> head $ lookupHqdmOne x joinInputModel) uniqueJoinNodes
    --print nodeTypeStatements

    -- Get the hqdmInputModel node ids of the types that are in the join Model
    let subtypes = lookupSubtypes hqdmInputModel

    let typeIdsOfJoinObjects = zip uniqueJoinNodes (fmap (head . lookupHqdmIdsFromTypePredicates hqdmInputModel . object)  nodeTypeStatements)
    -- Now filter the objects to join to be only those that are subtypes of ste
    let onlySubtypesOfSte = subtypesOfFilter typeIdsOfJoinObjects spatio_temporal_extent subtypes
    --print(isSubtype (snd $ head typeIdsOfJoinObjects) spatio_temporal_extent subtypes )

    let elementOfTypeName = getRelationNameFromRels elementOfType relationsInputModel
    let elementOfTypeTriples = fmap (\ x -> HqdmTriple (fst x) elementOfTypeName (snd x)) onlySubtypesOfSte

    --putStr "\nNew element_of_type triples\n\n"
    --print elementOfTypeTriples

    --putStr "\nNumber of element_of_type predicates is:\n\n"
    --print (length elementOfTypeTriples)

    let onlySubtypesOfClass = subtypesOfFilter typeIdsOfJoinObjects hqdmClass subtypes
    let hasSuperClassName = getRelationNameFromRels hasSuperclass relationsInputModel
    let hasSuperclassTriples = fmap (\ x -> HqdmTriple (fst x) hasSuperClassName (snd x)) onlySubtypesOfClass

    --putStr "\nNew class triples\n\n"
    --print hasSuperclassTriples

    --putStr "\nNumber of new class predicates is:\n\n"
    --print (length hasSuperclassTriples)

    -- Join the triples
    --putStr "\nDo resulting list lengths sum to the input length?\n\n"
    --print ((length hasSuperclassTriples + length elementOfTypeTriples) == length uniqueJoinNodes)

    --putStr "\nNow do the join and show the results:\n\n"

    let joinedResults = sortOnUuid $ joinInputModel ++ hasSuperclassTriples ++ elementOfTypeTriples
    --putStr (concat $ HqdmLib.csvTriplesFromHqdmTriples joinedResults)

    --putStr "\nResults length:"
    --print (length joinedResults)

    let allRelationIdJoinedTriples =  sortOnUuid $ hqdmSwapAnyRelationNamesForIdsStrict joinedResults hqdmInputModel relationsInputModel

    --putStr "\nExport the joined model all with predicates as Relation Ids:\n\n"
    putStr (concat $ HqdmLib.csvTriplesFromHqdmTriples allRelationIdJoinedTriples )


    ----------------------------------------
    -- LOAD Pre-Joined Triples
    ----------------------------------------
    preJoinedModel <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile "joinedAllRelsTestStrict.csv"
    let allRelationIdJoinedTriples = fromRight [] preJoinedModel

    let exampleObjectId = "7e181b8d-0aed-46ee-928e-b08d60d0ed58"
    let exampleObjectTriples = lookupHqdmOne exampleObjectId allRelationIdJoinedTriples

    -- Type
    let exampleObjectType = getTypeIdFromObject exampleObjectTriples hqdmInputModel
    --putStr ("\n\nExample object type id: " ++ exampleObjectType ++ "\n\n")

    -- What BR sets apply to this type?
    let exampleObjectTypeBRelSets = findBrelsFromDomain exampleObjectType relationsInputModel
    --print exampleObjectTypeBRelSets

    -- Query for set membership of an element
    --putStr "\n\nSet membership predicates:\n\n"
    let setMemberships = filterRelsBySet exampleObjectTriples relationsInputModel
    --print setMemberships

    -- Query for members of a set   

    -- Query for part-hood relations of an element
    --putStr "\n\nPart predicates:\n\n"
    let partPredicates = filterRelsByPart exampleObjectTriples relationsInputModel
    --print partPredicates

    --- Now find things that are part of it... and then trasitively

    --- Check for Cardiality violations

    let testResult = relationSetCheck (cardinalityMet exampleObjectTriples (head exampleObjectTypeBRelSets)) (head exampleObjectTypeBRelSets)
    --putStr ("\n\nCardinality Check:\n\n" ++  show testResult)

    --let testResultSet = cardinalityMetAllRels exampleObjectTriples exampleObjectTypeBRelSets
    --putStr ("\n\nCardinality Check Set:\n\n" ++  show testResultSet)

    let testResultAll = cardinalityTestAllObjects uniqueJoinNodes allRelationIdJoinedTriples hqdmInputModel relationsInputModel []
    let invalidResuls = validityFilter testResultAll
    let filteredResults = filterOutErrorsBy (head $ findBrelFromId "7b3caec7-7e9d-47cd-bb19-19d2872c326f" relationsInputModel) relationsInputModel invalidResuls
    --putStr "\n\nFailed tests filtered to remove parthood relations = \n"
    --putStr (printableErrorResults filteredResults hqdmInputModel allRelationIdJoinedTriples)

    putStr "\n\nRange Check:\n\n"

    let rangeTestResultsAll = rangeTestAllObjects uniqueJoinNodes allRelationIdJoinedTriples hqdmInputModel relationsInputModel []
    let invalidRangeResults = validityFilter rangeTestResultsAll
    
    putStr "\n\nFailed range tests = \n"
    --putStr (printableErrorResults invalidRangeResults hqdmInputModel allRelationIdJoinedTriples)

    putStr "\n\nSingle test of rangeMetTest:\n\n"
    -- point_in_time 6bb0b0b6-41cd-4bb3-a0e8-d483b25f6cf1
    let simpleRangeTest = rangeMetTest (HqdmLib.lookupHqdmOne "8883c70b-bc98-4c5f-b65d-92a21658947c" allRelationIdJoinedTriples) allRelationIdJoinedTriples hqdmInputModel (head $ findBrelFromId "8ea62706-fa07-40d7-8586-a8768403c01e" relationsInputModel)
    print simpleRangeTest

    putStr "\n\nSubBrel Tree mermaid example:\n\n"
    --let subBrelTree = findSubBRelTreeWithCount [[universalRelationSet]] relationsInputModel 2
    let relId = "be900942-8601-4254-9a12-d87a5bfa05d3" -- "7b3caec7-7e9d-47cd-bb19-19d2872c326f" Part --"69b0e5b9-3be2-4ec3-a9a6-bb5b523d4b32" Attr --"85e78ac0-ec72-478f-9aac-cacb520290a0" Top --"2db5490e-01d0-491e-bd64-67ac616f65a0" Set -- "f533fac8-d228-4c10-8799-a26fe6ea16a4" Emergent -- "cfb37186-d2d6-48de-a418-6197bdf0a7b0" Order
    -- let mermaidSubBrelTree = mermaidAddTitle (mermaidTDTopAndTail (insertBRNodeName relId relationsInputModel ++ mermaidSubRelationPathsWithLayerCount [[relId]] relationsInputModel 2 "")) ("Sub-BRel graph for " ++ relId)
    -- putStr mermaidSubBrelTree

    putStr "\n\nInput Rel Set:\n\n"
    print exampleObjectTypeBRelSets

    ------------------------------------------------------
    -- Create a list of related brels from brel ids in the test files 
    -- Test using the rispResult variable below
    let testBrels = findBrelsFromIds ["319ac105-0d66-4a1e-bc26-21b57dd1102f", "5df20202-283c-4273-9551-456cc182dd0d"] relationsInputModel
    
    putStr "\n\nTest of relationInSupertypePaths....\n\n"
    --let rispResult = relationInSupertypePaths "7b3caec7-7e9d-47cd-bb19-19d2872c326f" [head exampleObjectTypeBRelSets] relationsInputModel False
    let rispResult = filterHigherLevelBrels testBrels relationsInputModel
    print rispResult

    -- Find the spo statements of the given object and test if any of <o> identities are connected from the object to it by a BRel in the BRel Subtree
    putStr "\n\nSubBrel Tree example:\n\n"
    let targetSubBrelTree = uniqueIds $ concat (findSubBRelTreeWithCount [[partOf]] relationsInputModel 100)
    print targetSubBrelTree

    -- Do a transitive parthood query for a given node
    let queryNode = "b6663c0f-73ac-42a4-a027-333047618cb9"   --"04c63471-7073-4e6d-8adf-2bfb9c890ba8" -- R1 "b6663c0f-73ac-42a4-a027-333047618cb9" -- Installed Line Card 1 in R1 "a46c340b-640a-443c-9b46-fd57a2690c9e"
    putStr "\n\n"
    print (HqdmLib.findHqdmNamesInList [queryNode] allRelationIdJoinedTriples)
    putStr "\n\nParthood transitive query from Left: \n\n"
    let transitiveResults = transitiveQueryFromLeft targetSubBrelTree allRelationIdJoinedTriples [queryNode] []
    print transitiveResults
    let namesOfNodes = HqdmLib.findHqdmNamesInList (concat transitiveResults) allRelationIdJoinedTriples
    print namesOfNodes

    putStr "\n\nParthood transitive query from Right: \n\n"
    let transitiveResults2 = transitiveQueryFromRight targetSubBrelTree allRelationIdJoinedTriples [queryNode] []
    print transitiveResults2
    let namesOfNodes2 = HqdmLib.findHqdmNamesInList (concat transitiveResults2) allRelationIdJoinedTriples
    print namesOfNodes2

    putStr "\n\nDONE\n\n"

