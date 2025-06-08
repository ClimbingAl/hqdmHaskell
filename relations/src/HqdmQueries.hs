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
    filterForObjectOfType,
    filterRelsBy,
    filterRelsByAttribute,
    filterRelsByBeginning,
    filterRelsByEnding,
    filterRelsByPart,
    filterRelsBySet,
    transitiveQueryFromLeft,
    transitiveQueryFromRight
)
where

import qualified HqdmRelations
import qualified HqdmLib

part::HqdmRelations.RelationId
part = "be900942-8601-4254-9a12-d87a5bfa05d3"

set::HqdmRelations.RelationId
set = "2db5490e-01d0-491e-bd64-67ac616f65a0"

order::HqdmRelations.RelationId
order = "cfb37186-d2d6-48de-a418-6197bdf0a7b0"

emergent::HqdmRelations.RelationId
emergent = "f533fac8-d228-4c10-8799-a26fe6ea16a4"

attribute::HqdmRelations.RelationId
attribute = "fe987366-a8ad-48fa-8821-73f54f6df180"

beginning::HqdmRelations.RelationId
beginning = "96c965a9-ec3e-47f2-b18e-b67147bc0873"

ending::HqdmRelations.RelationId
ending = "aee002be-0529-4b80-82b0-0a6bcca34e48"

hqdmTypeRel::String 
hqdmTypeRel = "7e249a64-9f13-47d3-a232-562a3d080198"

-- | filterRelsBy
-- Filter the given HqdmAllAsData joined triples by a given set of relations (relSet)
-- This filtering is done by finding the all the sub-binary relation sets of the given set
-- and returning only the joined triples that are ordered pairs from those sets.
filterRelsBy :: HqdmRelations.RelationId -> [HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsBy relSet tpls brels = go tpls
    where
        subBrels = concat $ HqdmRelations.findSubBinaryRelationTree [[relSet]] brels

        go tpls = [values | values <- tpls, HqdmLib.predicate values `elem` subBrels]

filterRelsByPart::[HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsByPart = filterRelsBy part

filterRelsBySet::[HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsBySet = filterRelsBy set

filterRelsByBeginning::[HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsByBeginning = filterRelsBy beginning

filterRelsByEnding::[HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsByEnding = filterRelsBy ending

filterRelsByAttribute::[HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
filterRelsByAttribute = filterRelsBy attribute

-- Named Object Type Filter
filterForObjectOfType::[HqdmLib.HqdmTriple] -> String -> [String] 
filterForObjectOfType tpls typeName = [HqdmLib.subject values | values <- tpls, (typeName == HqdmLib.object values) && (hqdmTypeRel == HqdmLib.predicate values)]

-- | transitiveQueryFromLeft
-- Recursively find all the connected ids based on a specified list of binary relations with nodeIds applied from the left (e.g. <a rel b>, the node given is treated as a)
--    relSet  : The ids of the binary relation sets that are included in the transitive search (may include the computed set of sub-binrary relation sets)
--    tpls    : The dataset that contains the objects (as triples) to be queried
--    newNodeIds : The nodeId (or nodeIds) that the query is being made from (initially this may just be a single node)
--    nodeIds : The accumulated list of results
transitiveQueryFromLeft::[HqdmRelations.RelationId] -> [HqdmLib.HqdmTriple] -> [HqdmLib.Id] -> [[HqdmLib.Id]] -> [[HqdmLib.Id]]
transitiveQueryFromLeft relSet tpls newNodeIds nodeIds = go relSet tpls nodeIds
  where
    connectedNodes = [HqdmLib.object values | values <- tpls, (HqdmLib.predicate values  `elem` relSet) && (HqdmLib.subject values `elem` newNodeIds)]
    newNodes = HqdmLib.deleteItemsFromList connectedNodes (concat nodeIds) -- This is to make the function more efficient and add a defence against circularity

    go relSet tpls nodeIds
      | null newNodes = nodeIds
      | otherwise =  transitiveQueryFromLeft relSet tpls newNodes (nodeIds ++ [newNodes])

-- | transitiveQueryFromRight
-- Recursively find all the connected ids based on a specified list of binary relations with nodeIds applied from the right (e.g. <a rel b>, the node given is treated as b)
-- This is (roughly) equivalent to a transitive query for the inverse relations, if they were fully specified and used.
--    relSet  : The ids of the binary relation sets that are included in the transitive search (may include the computed set of sub-binrary relation sets)
--    tpls    : The dataset that contains the objects (as triples) to be queried
--    newNodeIds : The nodeId (or nodeIds) that the query is being made from (initially this may just be a single node)
--    nodeIds : The accumulated list of results
transitiveQueryFromRight::[HqdmRelations.RelationId] -> [HqdmLib.HqdmTriple] -> [HqdmLib.Id] -> [[HqdmLib.Id]] -> [[HqdmLib.Id]]
transitiveQueryFromRight relSet tpls newNodeIds nodeIds = go relSet tpls nodeIds
  where
    connectedNodes = [HqdmLib.subject values | values <- tpls, (HqdmLib.predicate values  `elem` relSet) && (HqdmLib.object values `elem` newNodeIds)]
    newNodes = HqdmLib.deleteItemsFromList connectedNodes (concat nodeIds) -- This is to make the function more efficient and add a defence against circularity

    go relSet tpls nodeIds
      | null newNodes = nodeIds
      | otherwise =  transitiveQueryFromRight relSet tpls newNodes (nodeIds ++ [newNodes])