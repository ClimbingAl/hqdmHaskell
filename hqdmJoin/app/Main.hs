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
    headListIfPresent,
    addNewCardinalitiesToPure,
    correctCardinalities,
    correctAllCardinalities,
    findMaxMaxCardinality,
    findMaxMinCardinality,
    hqdmSwapAnyRelationNamesForIds,
    printableLayerWithDomainAndRange,
    printablePathFromTuplesWithDomainAndRange, lookupSubBRelsOf
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

import HqdmIds

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.Either

-- Constants
hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../PureHqdmRelations_v7.csv"

hqdmInputFilename::String
hqdmInputFilename = "../HqdmAllAsDataFormal4Short.csv"

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
    let exampleObjectType = getTypeIdFromObject exampleObjectTriples hqdmInputModel
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

    let testResult = relationSetCheck (cardinalityMet exampleObjectTriples (head exampleObjectTypeBRelSets)) (head exampleObjectTypeBRelSets)
    putStr ("\n\nCardinality Check:\n\n" ++  show testResult)

    --let testResultSet = cardinalityMetAllRels exampleObjectTriples exampleObjectTypeBRelSets
    --putStr ("\n\nCardinality Check Set:\n\n" ++  show testResultSet)

    let testResultAll = cardinalityTestAllObjects uniqueJoinNodes allRelationIdJoinedTriples hqdmInputModel relationsInputModel []
    let invalidResuls = validityCheck testResultAll
    let filteredResults = filterOutErrorsBy (head $ findBrelFromId "7b3caec7-7e9d-47cd-bb19-19d2872c326f" relationsInputModel) relationsInputModel invalidResuls
    putStr "\n\nFailed tests filtered to remove parthood relations= \n"
    putStr (printableErrorResults filteredResults relationsInputModel hqdmInputModel allRelationIdJoinedTriples)

    {-putStr "\n\nInput Rel Set:\n\n"
    print exampleObjectTypeBRelSets
    
    putStr "\n\nTest of relationInSupertypePaths....\n\n"
    --let rispResult = relationInSupertypePaths "7b3caec7-7e9d-47cd-bb19-19d2872c326f" [head exampleObjectTypeBRelSets] relationsInputModel False
    let rispResult = filterHigherLevelBrels exampleObjectTypeBRelSets relationsInputModel
    print rispResult-}
    putStr "\n\nDONE\n\n"

printableErrorResults:: [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [HqdmBinaryRelationPure] -> [HqdmTriple] -> [HqdmTriple] -> String
printableErrorResults errs brels hqdm tpls =
    concatMap (\ x ->
        "\n\nObject Id:" ++ show (thdOf3 x) ++ " of type '" ++ head (lookupHqdmType $ lookupHqdmOne (thdOf3 x) tpls) ++ "'" ++
        "\nRelation result: " ++ show (fstOf3 x) ++ 
        onlyPrintInvalidTypeCause x ++
        printRelationWithTypeNames ( sndOf3 x) hqdm
        ) errs

onlyPrintInvalidTypeCause:: (CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id) -> String 
onlyPrintInvalidTypeCause err 
    | fstOf3 err == Valid = ""
    | otherwise = "\nLikely cause: Relationship missing.\n"

-- | filterOutErrorsBy x
-- Filter the list of cardinality results by the given super Brel Set
filterOutErrorsBy :: HqdmBinaryRelationPure -> [HqdmBinaryRelationPure] -> [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
filterOutErrorsBy brel brels errs = 
    [ values | values <- errs, not $ relationInSupertypePaths (getPureRelationId brel) [ sndOf3 values] brels False]

-- | filterHigherLevelBrels
-- Filter an input set of Binary Relation (Sets) to remove any that are on the super-BR path of others
-- This function approximately implements the redeclared (RT) keyword from EXPRESS
filterHigherLevelBrels :: [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
filterHigherLevelBrels brelSet brels =
    [ values | values <- brelSet, not $ relationInSupertypePaths (getPureRelationId values) brelSet brels False]

-- | relationInSupertypePaths
-- Returns True if the given relation is in the supertype path of one of the given set
relationInSupertypePaths :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure] -> Bool -> Bool
relationInSupertypePaths relId brelSet brels result = go relId brelSet brels result
    where
        superBRels = concat $ superRelationPathsToUniversalRelation [[getPureRelationId (head brelSet)]] brels
        idInSuperBRels = relId `elem` tail superBRels

        go relId brelSet brels result
            | null brelSet = result
            | otherwise = relationInSupertypePaths relId (tail brelSet) brels (result || idInSuperBRels)

validityCheck:: [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
validityCheck checkResults = [values | values <- checkResults,  fstOf3 values == Invalid]

-- | cardinalityTestAllObjects
cardinalityTestAllObjects:: [HqdmLib.Id] -> [HqdmTriple] -> [HqdmTriple] -> [HqdmBinaryRelationPure] -> [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
cardinalityTestAllObjects uuids tplsAll hqdm brels results = go uuids tplsAll hqdm brels results
    where
        uuid = head uuids
        objTpls = lookupHqdmOne uuid tplsAll
        typeId = getTypeIdFromObject objTpls hqdm
        typeBrels = findBrelsFromDomain typeId brels
        filteredBrels = filterHigherLevelBrels typeBrels brels

        go uuids tplsAll hqdm brels results
            | null uuids = results
            | otherwise = cardinalityTestAllObjects (tail uuids) tplsAll hqdm brels (results ++ cardinalityMetAllRels objTpls filteredBrels)

-- | getTypeIdFromObject
-- Get the Id of the Hqdm Type from a supplied set of triples for a joined Hqdm object ### Implement test for this
getTypeIdFromObject:: [HqdmTriple] -> [HqdmTriple] -> Id
getTypeIdFromObject objTpls hqdm = head $ lookupHqdmIdFromType hqdm ( head $ lookupHqdmType objTpls )

-- | cardinalityMetAllRels
-- Tests whether the collection of triples for a single Hqdm Node (object) satisfies 
-- all of the HqdmBinaryRelationPure relation sets for that type of object. 
cardinalityMetAllRels:: [HqdmTriple] -> [HqdmBinaryRelationPure] -> [(CardinalityCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
cardinalityMetAllRels _ [] = []
cardinalityMetAllRels [] _ = []
cardinalityMetAllRels tpls (brel : brels) = relationSetAndIdCheck (cardinalityMet tpls brel) brel (subject $ head tpls) : cardinalityMetAllRels tpls brels

-- | cardinalityMet
-- Tests whether the collection of triples for a single Hqdm Node (object) satisfies the
-- supplied HqdmBinaryRelationPure
cardinalityMet:: [HqdmTriple] -> HqdmBinaryRelationPure -> CardinalityCheck
cardinalityMet tpls brel = go tpls brel
    where
        brelId = getPureRelationId brel
        brelCMin = getPureCardinalityMin brel
        brelCMax = getPureCardinalityMax brel
        relCmp = fmap (\ x -> (oneOrZero (brelId == predicate x)::Int)) tpls
        relCount = sum relCmp

        go tpls brel
            | (relCount > brelCMax) && (brelCMax > 0) = Invalid
            | (relCount < brelCMin) && (brelCMin > 0) = Invalid
            | otherwise = Valid

oneOrZero:: Bool -> Int
oneOrZero val
    | val = 1
    | otherwise = 0

fstOf3 :: (a, b, c) -> a
fstOf3 (x, _, _) = x

sndOf3 :: (a, b, c) -> b
sndOf3 (_, x, _) = x

thdOf3 :: (a, b, c) -> c
thdOf3 (_, _, x) = x