{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  HqdmRelations
-- Description :  Module with functions to apply to HQDM AllAsData Triples
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Functions to apply to HQDM AllAsData triples including mapping to data types,
-- calculating subtype and supertype trees from given HQDM type Ids and calculating
-- inherited relations for given HQDM type Ids.
--
-- Functions also provided to render the outputs as printable text.

module HqdmRelations
  ( 
  )
where

import HqdmLib
  ( Id,
    HqdmTriple,
    RelationPair,
    Relation,
    HqdmHasSupertype,
    subject,
    getSubjects,
    getPredicates,
    uniqueIds,
    uniqueTriples,
    stringListSort,
    lookupHqdmOne,
    lookupHqdmType,
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
    csvTriplesFromHqdmTriples
  )

-- | In a RelationPairSet xR'y the  is a list of [R'y] for x, where R' can be any allowed 
--   number of isntances of permitted Relations
data HqdmRelationPairSet = HqdmRelationPairSet
  { id :: !Id,
    relationPairs :: [RelationPair]
  }
  deriving (Show, Eq, Generic)

-- | A 
data RelationPair = RelationPair
  { relationId :: !RelationId,
    binaryRelationId :: !RelationId,
    object :: !Id             -- The Id of the end of the relation (object of subject-predicate-object)
  }
  deriving (Show, Eq, Generic)

newtype BinaryRelation = Relation
  { relationId :: !RelationId,-- Relation unique Id (hqdmRel:uuid)
    relationName :: String,   -- Name of Binary Relation Set (doesn't need to be unique?)
    hasSuperBR :: Id,         -- SuperBR Set Id (empty if none?)
    rangeSet :: [Id],         -- Range Set ids.  Does this need to be a list?
    cardinalityMin :: Int,    -- 0,1,...
    cardinalityMax :: Int,    -- -1 (no max!),0,1,2,...
    redeclaredBR :: Bool      -- True means supertypes are abstract?
  }
  deriving (Show, Eq, Generic)

data HqdmBinaryRelationSet = HqdmBinaryRelationSet
  { id :: !Id,
    relationPairs :: [BinaryRelation]
  }
  deriving (Show, Eq, Generic)

type RelationId = String