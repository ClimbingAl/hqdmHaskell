{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  HqdmRelations
-- Description :  Module with Relation functions to apply to HQDM AllAsData Triples
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

module HqdmRelations
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
    getPureReclared,
    getPureRedeclaredFromRange,
    getBrelDomainFromRels,
    findBrelDomainSupertypes,
    findBrelFromId,
    superRelationPathToUniversalRelation,
    relIdNameTuples,
    printablePathFromTuples,
    findBrelsWithDomains,
    findBrelsAndNamesWithDomains,
    findSuperBinaryRelation,
    findSuperBinaryRelation',
    addStRelationToPure,
    printablePureRelation,
    csvRelationsFromPure,
    lookupSuperBinaryRelOf,
    hqdmSwapRelationNamesForIds,
    convertRelationByDomainAndRange,
    headListIfPresent,
    addNewCardinalitiesToPure
  )
where

import qualified HqdmLib
import GHC.Generics (Generic)
import Data.Csv (FromRecord)
import Data.List (isPrefixOf)

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

-- | HqdmBinaryRelation is structured to be compatable with the original HQDM EXPRESS 
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

data HqdmBinaryRelation = HqdmBinaryRelation
  { domain :: String,
    binaryRelationId :: !RelationId,  -- Relation unique Id (hqdmRel:uuid)
    binaryRelationName :: String,     -- Name of Binary Relation Set (doesn't need to be unique?)
    range:: String,                   -- Range Set ids.  Does this need to be a list?
    hasSuperBR :: String,             -- SuperBR Set Id (empty if none?)... Should be HqdmLib.Id
    cardinalityMin :: Int,            -- 0,1,...
    cardinalityMax :: Int,            -- -1 (no max!),0,1,2,...
    redeclaredBR :: String,              -- True means superBRtypes are abstract?
    redeclaredFromRange :: String
  }
  deriving (Show, Eq, Generic)

instance FromRecord HqdmBinaryRelation

data HqdmBinaryRelationSet = HqdmBinaryRelationSet
  { nodeId :: !HqdmLib.Id,
    binaryRelationPairs :: [HqdmBinaryRelation]
  }
  deriving (Show, Eq, Generic)

type RelationId = String

-- | HqdmBinaryRelationPure
-- A data type that uses only identities to specify the xR'y of a HQDM Binary 
data HqdmBinaryRelationPure = HqdmBinaryRelationPure
  { pureDomain :: !HqdmLib.Id,
    pureBinaryRelationId :: !RelationId,  -- Relation unique Id (hqdmRel:uuid)
    pureBinaryRelationName :: String,     -- Name of Binary Relation Set (doesn't need to be unique?)
    pureRange:: !HqdmLib.Id,              -- Range Set ids.  Does this need to be a list?
    pureHasSuperBR :: HqdmLib.Id,         -- SuperBR Set Id (empty if none?)... Should be HqdmLib.Id
    pureCardinalityMin :: Int,            -- 0,1,...
    pureCardinalityMax :: Int,            -- -1 (indicates no max!),0,1,2,...
    pureRedeclaredBR :: Bool,              -- True means superBRtypes are abstract?
    pureRedeclaredFromRange :: HqdmLib.Id
  }
  deriving (Show, Eq, Generic)

universalRelationSet::String
universalRelationSet = "hqdmRelation:85e78ac0-ec72-478f-9aac-cacb520290a0"

hqdmType::String
hqdmType = "hqdm:type"

hqdmHasSupertype::String
hqdmHasSupertype = "hqdm:has_supertype"

hqdmHasSuperclass::String
hqdmHasSuperclass = "hqdm:has_superclass"

getPureDomain :: HqdmBinaryRelationPure -> RelationId
getPureDomain = pureDomain

getPureRelationId :: HqdmBinaryRelationPure -> RelationId
getPureRelationId = pureBinaryRelationId

getPureRelationName :: HqdmBinaryRelationPure -> String
getPureRelationName = pureBinaryRelationName

getPureRange :: HqdmBinaryRelationPure -> RelationId
getPureRange = pureRange

getPureSuperRelation :: HqdmBinaryRelationPure -> RelationId
getPureSuperRelation = pureHasSuperBR

getPureCardinalityMin :: HqdmBinaryRelationPure -> Int
getPureCardinalityMin = pureCardinalityMin

getPureCardinalityMax :: HqdmBinaryRelationPure -> Int
getPureCardinalityMax = pureCardinalityMax

getPureReclared :: HqdmBinaryRelationPure -> Bool
getPureReclared = pureRedeclaredBR

getPureRedeclaredFromRange :: HqdmBinaryRelationPure -> RelationId
getPureRedeclaredFromRange = pureRedeclaredFromRange

stringToBool :: String -> Bool
stringToBool x
  | x=="True" = True
  | otherwise = False

headIfStringPresent :: [String] -> String
headIfStringPresent x
  | not (null x)   = head x
  | otherwise      = ""

hqdmRelationsToPure :: [HqdmBinaryRelation] -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure]
hqdmRelationsToPure brels tpls = fmap ( \ x -> HqdmBinaryRelationPure
  ( headIfStringPresent (HqdmLib.lookupHqdmIdFromType tpls (domain x) ) )
  ( binaryRelationId x)
  ( binaryRelationName x)
  ( headIfStringPresent (HqdmLib.lookupHqdmIdFromType tpls (range x) ) )
  ( headIfStringPresent (HqdmLib.lookupHqdmIdFromType tpls (hasSuperBR x) ) )
  ( cardinalityMin x)
  ( cardinalityMax x)
  ( stringToBool (redeclaredBR x))
  ( headIfStringPresent (HqdmLib.lookupHqdmIdFromType tpls (redeclaredFromRange x) )) )  brels

csvRelationsToPure :: [HqdmBinaryRelation] -> [HqdmBinaryRelationPure]
csvRelationsToPure = fmap ( \ x -> HqdmBinaryRelationPure
  ( domain x )
  ( binaryRelationId x)
  ( binaryRelationName x)
  ( range x )
  ( hasSuperBR x )
  ( cardinalityMin x)
  ( cardinalityMax x)
  ( stringToBool (redeclaredBR x))
  ( redeclaredFromRange x ))

getRelationNameFromRels :: RelationId -> [HqdmBinaryRelationPure] -> String
getRelationNameFromRels relId brels = headIfStringPresent [pureBinaryRelationName values | values <- brels, relId == pureBinaryRelationId values]

getBrelDomainFromRels :: RelationId -> [HqdmBinaryRelationPure] -> HqdmLib.Id
getBrelDomainFromRels relId brels = headIfStringPresent [pureDomain values | values <- brels, relId == pureBinaryRelationId values]

findBrelDomainSupertypes :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> [HqdmLib.Id]
findBrelDomainSupertypes relId brels = HqdmLib.lookupSupertypeOf (getBrelDomainFromRels relId brels)

findBrelFromId :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
findBrelFromId relId brels = take 1 [values | values <- brels, relId == pureBinaryRelationId values]

superRelationPathToUniversalRelation :: RelationId -> [HqdmBinaryRelationPure] -> [RelationId] -> [RelationId]
superRelationPathToUniversalRelation relId brels relPath = go brels relPath
  where
    superBR = getPureSuperRelation $ head (findBrelFromId relId brels)

    go brels relPath
      | null superBR = relPath
      | otherwise = superRelationPathToUniversalRelation superBR brels (relPath ++ [superBR])

relIdNameTuples :: [RelationId] -> [HqdmBinaryRelationPure] -> [(RelationId, String)]
relIdNameTuples relIds brels = fmap (\ x -> (x, head [pureBinaryRelationName values | values <- brels, x == pureBinaryRelationId values])) relIds

printablePathFromTuples :: [(RelationId, String)] -> String
printablePathFromTuples tpls = concatMap (\ x -> HqdmLib.fmtString (snd x ) ++ ",\n" ++ HqdmLib.fmtString (fst x) ++ "\n" ++ HqdmLib.fmtString "^\n" ++ HqdmLib.fmtString "|\n")  (reverse tpls)

hqdmSwapRelationNamesForIds :: [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
hqdmSwapRelationNamesForIds hqdmTpls brels =
  fmap (`convertRelationByDomainAndRange` brels) hqdmTpls

convertRelationByDomainAndRange :: HqdmLib.HqdmTriple -> [HqdmBinaryRelationPure] -> HqdmLib.HqdmTriple
convertRelationByDomainAndRange tpl brels = go tpl
  where
    pureRelMatch = headIfStringPresent [ pureBinaryRelationId values | values <- brels, (HqdmLib.subject tpl == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) ]
    hqdmTypeBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmType == pureBinaryRelationName values ]
    hqdmHasSupertypeBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSupertype == pureBinaryRelationName values ]
    hqdmHasSuperclassBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSuperclass == pureBinaryRelationName values ]

    go tpl
      | HqdmLib.predicate tpl==hqdmType = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSupertype = HqdmLib.HqdmTriple (HqdmLib.subject tpl)  hqdmHasSupertypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSuperclass  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasSuperclassBR (HqdmLib.object tpl)
      | otherwise = HqdmLib.HqdmTriple (HqdmLib.subject tpl) pureRelMatch (HqdmLib.object tpl)

{-convertRelationByDomainRangeAndName tpl brels = HqdmLib.HqdmTriple
  (HqdmLib.subject tpl)
  (headIfStringPresent [ pureBinaryRelationId values | values <- brels, (HqdmLib.subject tpl == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) ])
  (HqdmLib.object tpl)-}

--------------------------------- MESSY RELATION ASSEMBLY FUNCTIONS FROM SOURE HqdmAllAsData TRIPLES --------------------------------------------
-- | findBrelsWithDomains
-- Takes a list of domain Ids (e.g. from findBrelDomainSupertypes) and finds associated RelationIds
findBrelsWithDomains :: [HqdmLib.Id] -> [HqdmBinaryRelationPure] -> [RelationId]
findBrelsWithDomains ids brels = concatMap (\ x -> [pureBinaryRelationId values | values <- brels, x == pureDomain values]) ids

findBrelsAndNamesWithDomains :: [HqdmLib.Id] -> [HqdmBinaryRelationPure] -> [(RelationId, String)]
findBrelsAndNamesWithDomains ids brels = zip (findBrelsWithDomains ids brels) (fmap (`getRelationNameFromRels` brels) (findBrelsWithDomains ids brels))

-- | findSuperBinaryRelation
-- Takes a type RelationId, finds its domain's supertype(s) and returns the closest match(es?)
findSuperBinaryRelation :: RelationId -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [(RelationId, String)]
findSuperBinaryRelation relId tpls brels = goFind relId tpls brels
  where
    relName = getRelationNameFromRels relId brels
    subtypes = HqdmLib.lookupSubtypes tpls
    domainSupertypesOfRel = findBrelDomainSupertypes relId brels subtypes
    namesOfBrelsOfDomain = findBrelsAndNamesWithDomains domainSupertypesOfRel brels
    closestNameMatches = [x | x <- namesOfBrelsOfDomain, snd x `isPrefixOf` relName]

    goFind relId tpls brels
      | null relId = []
      | null tpls = []
      | null brels = []
      | otherwise = closestNameMatches


headListIfPresent :: [a] -> Maybe a
headListIfPresent []     = Nothing
headListIfPresent (a:as) = Just a

getRelationIdFromMonadTuple :: Maybe (RelationId, String) -> String
getRelationIdFromMonadTuple = maybe "" fst

findSuperBinaryRelation' :: RelationId -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> Maybe (RelationId, String)
findSuperBinaryRelation' relId tpls brels =
  headListIfPresent [x | x <-
    findBrelsAndNamesWithDomains
      (findBrelDomainSupertypes relId brels (HqdmLib.lookupSubtypes tpls)) brels,
        snd x `isPrefixOf` getRelationNameFromRels relId brels]

addStRelationToPure :: Maybe (RelationId, String) -> HqdmBinaryRelationPure -> HqdmBinaryRelationPure
addStRelationToPure stRel x = HqdmBinaryRelationPure
  ( pureDomain x)
  ( pureBinaryRelationId x)
  ( pureBinaryRelationName x)
  ( pureRange x)
  ( getRelationIdFromMonadTuple stRel )
  ( pureCardinalityMin x)
  ( pureCardinalityMax x)
  ( pureRedeclaredBR x)
  ( pureRedeclaredFromRange x)

addNewCardinalitiesToPure :: Int -> Int -> HqdmBinaryRelationPure -> HqdmBinaryRelationPure
addNewCardinalitiesToPure cardMin cardMax x = HqdmBinaryRelationPure
  ( pureDomain x)
  ( pureBinaryRelationId x)
  ( pureBinaryRelationName x)
  ( pureRange x)
  ( pureHasSuperBR x )
    cardMin
    cardMax
  ( pureRedeclaredBR x)
  ( pureRedeclaredFromRange x)

comma::String
comma = ","

boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

-- | printablePureRelation
-- Printable pure Relation for export as CSV
printablePureRelation :: HqdmBinaryRelationPure -> String
printablePureRelation x =
  pureDomain x ++ comma ++
  pureBinaryRelationId x ++ comma ++
  pureBinaryRelationName x ++ comma ++
  pureRange x ++ comma ++
  pureHasSuperBR x ++ comma ++
  show (pureCardinalityMin x) ++ comma ++
  show (pureCardinalityMax x) ++ comma ++
  boolToString (pureRedeclaredBR x) ++ comma ++
  pureRedeclaredFromRange x ++ "\n"

csvRelationsFromPure :: [HqdmBinaryRelationPure] -> String
csvRelationsFromPure = concatMap printablePureRelation

-- | lookupSuperBinaryRelOf
-- From all the Pure Binary Relations given find the super Binary Relation of a given RelationId.
lookupSuperBinaryRelOf :: RelationId -> [HqdmBinaryRelationPure] -> Maybe RelationId
lookupSuperBinaryRelOf x brList = headListIfPresent [pureHasSuperBR values | values <- brList, x == pureBinaryRelationId values]
