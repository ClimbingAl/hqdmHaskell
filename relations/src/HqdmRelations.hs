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
    object :: !Id
  }
  deriving (Show, Eq, Generic)

newtype BinaryRelation = Relation
  { relationId :: !RelationId,
    relationName :: String,
    hasSuperBR :: Id,
    rangeSet :: [Id],
    cardinalityMin :: Int,
    cardinalityMax :: Int
  }
  deriving (Show, Eq, Generic)

data HqdmBinaryRelationSet = HqdmBinaryRelationSet
  { id :: !Id,
    relationPairs :: [BinaryRelation]
  }
  deriving (Show, Eq, Generic)

type RelationId = String