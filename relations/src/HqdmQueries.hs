{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- Module      :  HqdmQueries
-- Description :  Module with Query functions to apply to HQDM AllAsData Triples
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Functions to apply to HQDM AllAsData triples and associated Binary Relations
-- encoded separately. Some functions are not intended for regular use but are
-- provided for loading the original Relation specifications from HQDM.exp. 
--
-- Functions also provided to render the outputs as printable text.

module HqdmQueries (
    part,
    set,
    order,
    emergent,
    filterRelsBy,
    filterRelsByPart,
    filterRelsBySet
)
where

import qualified HqdmRelations
  (
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
    hqdmSwapAnyRelationNamesForIdsStrict,
    printableLayerWithDomainAndRange,
    printablePathFromTuplesWithDomainAndRange,
    findSubBinaryRelationTree,
    lookupSubBRelsOf,
    lookupSubBRelOf
  )

import qualified HqdmLib (
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
    fmtString,
    deleteItemsFromList
    )

import GHC.Generics (Generic)
import Data.Csv (FromRecord)
import Data.List (isPrefixOf, sortOn)

part::HqdmRelations.RelationId
part = "7b3caec7-7e9d-47cd-bb19-19d2872c326f"

set::HqdmRelations.RelationId
set = "2db5490e-01d0-491e-bd64-67ac616f65a0"

order::HqdmRelations.RelationId
order = "cfb37186-d2d6-48de-a418-6197bdf0a7b0"

emergent::HqdmRelations.RelationId
emergent = "f533fac8-d228-4c10-8799-a26fe6ea16a4"

-- | filterRelsBy
-- Filter the given HqdmAllAsData joined triples by a given set of relations (relSet)
-- This filtering is done by finding the all the sub-binary relation sets of the given set
-- and returning only the joined triples that are ordered pairs from one or more of those
-- sets.
-- NOTE: Only works where predicates are converted to HqdmLib.RelationId
filterRelsBy :: HqdmRelations.RelationId -> [HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsBy relSet tpls brels = go tpls
    where
        subBrels = concat $ HqdmRelations.findSubBinaryRelationTree [[relSet]] brels
        go tpls = [values | values <- tpls, HqdmLib.predicate values `elem` subBrels]

filterRelsByPart::[HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsByPart = filterRelsBy part

filterRelsBySet::[HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsBySet = filterRelsBy set


