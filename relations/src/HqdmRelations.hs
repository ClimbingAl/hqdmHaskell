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

import qualified HqdmLib
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
import GHC.Generics (Generic)

-- | In a RelationPairSet xR'y the  is a list of [R'y] for x, where R' can be any allowed 
--   number of instances of permitted Relations
data HqdmRelationSet = HqdmRelationSet
  { relationshipId :: !HqdmLib.Id,
    relationPairs :: ![RelationPair]
  }
  deriving (Show, Eq, Generic)

-- | A RelationPair is strictly an instance of a relationship R'y
--   (as a first class object; an element of a Binary Relation SET).
--   This is the 'right-hand' part of an ordered pair, with the 'left-hand'
--   part being the NodeId (i.e. the x in xR'y, and subject, in s-p-o parlance) 
--   of the relationship.
data RelationPair = RelationPair
  { relationId :: !RelationId,
    binaryRelationSetId :: !RelationId,
    object :: !HqdmLib.Id             -- The Id of the end of the relation (object of subject-predicate-object)
  }
  deriving (Show, Eq, Generic)

-- | BinaryRelation is structured to be compatable with the original HQDM EXPRESS 
--   documentation (hqdm.exp) and the python extractor created to extract the following
--   properties of each relation specified in the EXPRESS data file.  A uuid is applied
--   to each specification for a binary relation in HQDM EXPRESS.  This should remain
--   fixed in the source hqdmRelations.csv.
--
-- Note: The has_supertype relations are not covered by this but can be added by hand.
--       This is because the EXPRESS notation handles super/su-types as a separate 
--       keyword (e.g. "SUBTYPE OF")
--
--  class Relation:
--        self.domain = ""            # Name of node that is the x in xR'y Binary Relation
--
--        self.relationId = ""        # !RelationId,-- Relation unique Id (hqdmRel:uuid)
--        self.relationName = ""      # Name of Binary Relation Set (doesn't need to be unique?)
--        self.rangeSet = ""          # Range Set id that is the y in xR'y ()
--        self.hasSuperBR = ""        # SuperBR Set Id (empty if none?)
--        self.cardinalityMin = 0     # 0,1,...
--        self.cardinalityMax = -1    # -1 (no max!),0,1,2,...
--        self.redeclaredBR = False   # True means superBRtypes are abstract?
--        self.inverseOf = ""         # Inverse of named relation

data BinaryRelation = BinaryRelation
  { binaryRelationId :: !RelationId,  -- Relation unique Id (hqdmRel:uuid)
    binaryRelationName :: String,     -- Name of Binary Relation Set (doesn't need to be unique?)
    range:: !HqdmLib.Id,              -- Range Set ids.  Does this need to be a list?
    hasSuperBR :: HqdmLib.Id,        -- SuperBR Set Id (empty if none?)
    cardinalityMin :: Int,            -- 0,1,...
    cardinalityMax :: Int,            -- -1 (no max!),0,1,2,...
    redeclaredBR :: Bool              -- True means superBRtypes are abstract?
  }
  deriving (Show, Eq, Generic)

data HqdmBinaryRelationSet = HqdmBinaryRelationSet
  { nodeId :: !HqdmLib.Id,
    binaryRelationPairs :: [BinaryRelation]
  }
  deriving (Show, Eq, Generic)

type RelationId = String