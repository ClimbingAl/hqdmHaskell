{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BangPatterns #-}

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
    IdMapping,
    uuidToInt,
    intToUuid,
    RelationIntIndex,
    buildIdMapping,
    buildIndexDownFastest,
    findSubBinaryRelationTreeFastest,
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
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.UUID ( UUID )

type RelationIntIndex = IntMap.IntMap IntSet.IntSet

--buildIndexUp :: [HqdmBinaryRelationPure] -> RelationIndex
--buildIndexUp rels = Map.fromListWith (++) [ (pureBinaryRelationId r, pureHasSuperBR r) | r <- rels ]

buildIndexDownFastest :: IdMapping -> [HqdmRelations.HqdmBinaryRelationPure] -> RelationIntIndex
buildIndexDownFastest mapping rels = IntMap.fromListWith IntSet.union
  [ (superIntId, IntSet.singleton childIntId)
  | r <- rels
  , let lookupInt uid = Map.findWithDefault (-1) uid (uuidToInt mapping)
  , let childIntId = lookupInt (HqdmRelations.pureBinaryRelationId r)
  , childIntId /= -1 -- Safely filter out unmapped IDs if any mismatch occurs
  , superUuid <- HqdmRelations.pureHasSuperBR r
  , let superIntId = lookupInt superUuid
  , superIntId /= -1
  ]

data IdMapping = IdMapping
  { uuidToInt :: !(Map.Map Data.UUID.UUID Int)
  , intToUuid :: !(IntMap.IntMap Data.UUID.UUID)
  }

buildIdMapping :: [HqdmRelations.HqdmBinaryRelationPure] -> IdMapping
buildIdMapping rels = IdMapping toInt toUuidFast
  where
    -- Gather every unique UUID present in both relation IDs and super lists
    allUuids = Map.keys $ Map.fromList
      [ (uid, ())
      | r <- rels
      , uid <- HqdmRelations.pureBinaryRelationId r : HqdmRelations.pureHasSuperBR r
      ]

    -- Pair each unique UUID with a sequential integer index [0..]
    zipped = zip allUuids [0..]

    toInt  = Map.fromList zipped
    toUuidFast = IntMap.fromList [ (i, u) | (u, i) <- zipped ]

-- Helper
findSubBinaryRelationTreeUUID :: IdMapping -> RelationIntIndex -> [UUID] -> [[UUID]]
findSubBinaryRelationTreeUUID mapping trieIdx incomingUuidLayers =
  let 
    -- Helper: Look up UUID -> Int
    toInt uid = Map.findWithDefault (-1) uid (uuidToInt mapping)
    -- Helper: Look up Int -> UUID
    toUuid i  = IntMap.findWithDefault (error "Inconsistent Mapping Key") i (intToUuid mapping)
    
    -- 1. Convert incoming boundary layout from UUID matrix down to Int matrix
    initialIntLayers = map toInt incomingUuidLayers
    
    -- 2. Run the ultra-fast bitwise Trie query loop
    resultingIntLayers = findSubBinaryRelationTreeFastest trieIdx initialIntLayers
  in
    -- 3. Re-inflate the packed integer results back up to safe UUIDs
    map (map toUuid) resultingIntLayers


findSubBinaryRelationTreeFastest :: RelationIntIndex -> [Int] -> [[Int]]
findSubBinaryRelationTreeFastest index initialIntIds =
    reverse (go initialLayers initialVisited (last initialLayers))
  where
    initialLayers  = reverse [initialIntIds]
    initialVisited = IntSet.fromList initialIntIds

    go :: [[Int]] -> IntSet.IntSet -> [Int] -> [[Int]]
    go !accLayers !visited !currentLayer
      | null currentLayer = accLayers
      | IntSet.null filteredNewLayerSet = accLayers
      | otherwise = go (filteredNewLayer : accLayers) nextVisited filteredNewLayer
      where
        -- Instant hardware-level bitwise lookups and structural unions
        allPossibleChildren = IntSet.unions
          [ IntMap.findWithDefault IntSet.empty pId index | pId <- currentLayer ]

        -- High-speed bitfield exclusion replaces heavy binary tree sorting
        filteredNewLayerSet = IntSet.difference allPossibleChildren visited
        filteredNewLayer    = IntSet.toList filteredNewLayerSet
        nextVisited         = IntSet.union visited filteredNewLayerSet

-- | filterRelsBy
-- Filter the given HqdmAllAsData joined triples by a given set of relations (relSet)
-- This filtering is done by finding the all the sub-binary relation sets of the given set
-- and returning only the joined triples that are ordered pairs from those sets.
fastFilterRelsBy :: HqdmRelations.RelationId -> [HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsBy relSet tpls mapping trieIdx = go tpls
    where
        subBrels = concat $ findSubBinaryRelationTreeUUID mapping trieIdx [relSet]

        go tpls = [values | values <- tpls, HqdmLib.predicate values `elem` subBrels]

fastFilterRelsByPart::[HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByPart = fastFilterRelsBy HqdmQueries.part

fastFilterRelsBySet::[HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsBySet = fastFilterRelsBy HqdmQueries.set

fastFilterRelsByBeginning::[HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByBeginning = fastFilterRelsBy HqdmQueries.beginning

fastFilterRelsByEnding::[HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByEnding = fastFilterRelsBy HqdmQueries.ending

fastFilterRelsByAttribute::[HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByAttribute = fastFilterRelsBy HqdmQueries.attribute

fastFilterRelsByNameAttribute::[HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByNameAttribute = fastFilterRelsBy HqdmQueries.nameAttribute

-- fastFilterRelsBy BrelId and RangeId
fastFilterRelsByBrelAndRangeId :: HqdmRelations.RelationId -> HqdmLib.Id -> [HqdmLib.HqdmTriple] -> IdMapping -> RelationIntIndex -> [HqdmLib.HqdmTriple]
fastFilterRelsByBrelAndRangeId relSet rangeId tpls mapping trieIdx = go tpls
    where
        subBrels = concat $ findSubBinaryRelationTreeUUID mapping trieIdx [relSet]

        go tpls = [values | values <- tpls, (HqdmLib.predicate values `elem` subBrels) && (HqdmLib.object values == rangeId)]
