{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- Module      :  HqdmQueriesFast
-- Description :  Module with Query functions to apply to HQDM AllAsData Triples
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- As for HqdmQueries but with added IntSet mappings for faster performance 
--
-- Functions also provided to render the outputs as printable text.

module HqdmQueriesFast (
    findSubBinaryRelationTreeUUID,
    fastFilterRelsBy,
    fastFilterRelsByAttribute,
    fastFilterRelsByNameAttribute,
    fastFilterRelsByBeginning,
    fastFilterRelsByEnding,
    fastFilterRelsByPart,
    fastFilterRelsBySet,
    fastFilterRelsByBrelAndRangeId,
)
where

import qualified HqdmRelations
import qualified HqdmLib
import qualified HqdmQueries
import Data.String (String)
import Data.UUID (UUID, fromString, toString)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

-- Helpers
findSubBinaryRelationTreeUUID :: HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [UUID] -> [[UUID]]
findSubBinaryRelationTreeUUID mapping trieIdx incomingUuidLayers =
  let 
    -- Helper: Look up UUID -> Int
    toInt uid = Map.findWithDefault (-1) uid (HqdmRelations.uuidToInt mapping)
    -- Helper: Look up Int -> UUID
    toUuid i  = IntMap.findWithDefault (error "Inconsistent Mapping Key") i (HqdmRelations.intToUuid mapping)
    
    -- 1. Convert incoming boundary layout from UUID matrix down to Int matrix
    initialIntLayers = map toInt incomingUuidLayers
    
    -- 2. Run the ultra-fast bitwise Trie query loop
    resultingIntLayers = HqdmRelations.findSubBinaryRelationTreeFastest mapping trieIdx initialIntLayers
  in
    -- 3. Re-inflate the packed integer results back up to safe UUIDs
    map (map toUuid) resultingIntLayers

-- | filterRelsBy
-- Filter the given HqdmAllAsData joined triples by a given set of relations (relSet)
-- This filtering is done by finding the all the sub-binary relation sets of the given set
-- and returning only the joined triples that are ordered pairs from those sets.
fastFilterRelsBy :: HqdmRelations.RelationId -> [HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsBy relSet tpls mapping trieIdx = go tpls
    where
        subBrels = concat $ findSubBinaryRelationTreeUUID mapping trieIdx [relSet]

        go tpls = [values | values <- tpls, HqdmLib.predicate values `elem` subBrels]

fastFilterRelsByPart::[HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByPart = fastFilterRelsBy HqdmQueries.part

fastFilterRelsBySet::[HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsBySet = fastFilterRelsBy HqdmQueries.set

fastFilterRelsByBeginning::[HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByBeginning = fastFilterRelsBy HqdmQueries.beginning

fastFilterRelsByEnding::[HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByEnding = fastFilterRelsBy HqdmQueries.ending

fastFilterRelsByAttribute::[HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByAttribute = fastFilterRelsBy HqdmQueries.attribute

fastFilterRelsByNameAttribute::[HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByNameAttribute = fastFilterRelsBy HqdmQueries.nameAttribute

-- fastFilterRelsBy BrelId and RangeId
fastFilterRelsByBrelAndRangeId :: HqdmRelations.RelationId -> HqdmLib.Id -> [HqdmLib.HqdmTriple] -> HqdmRelations.IdMapping -> HqdmRelations.RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByBrelAndRangeId relSet rangeId tpls mapping trieIdx = go tpls
    where
        subBrels = concat $ findSubBinaryRelationTreeUUID mapping trieIdx [relSet]

        go tpls = [values | values <- tpls, (HqdmLib.predicate values `elem` subBrels) && (HqdmLib.object values == rangeId)]
