{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
-- HQDM AllAsData Triples are now handled using the Haskell Data.UUID data type.

module HqdmRelations
  (
    RelationId,
    RelationPair,
    HqdmRelationSet,
    HqdmBinaryRelationSet,
    HqdmBinaryRelationPure(..),
    RelationCheck (Valid, Invalid),
    relationSetCheck,
    relationSetAndIdCheck,
    universalRelationSet,
    getRelationNameFromRels,
    getPureDomain,
    getPureRelationId,
    getPureRelationName,
    getPureRange,
    getPureSuperRelation,
    getPureSuperRelations,
    getPureCardinalityMin,
    getPureCardinalityMax,
    getPureRedeclared,
    printRelation,
    printRelationWithTypeNames,
    getBrelDomainFromRels,
    getBrelRangeFromRels,
    findBrelDomainSupertypes,
    findBrelFromId,
    findBrelsFromIds,
    findBrelsFromDomain,
    findBrelsFromRange,
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
    headListIfPresent,
    addNewCardinalitiesToPure,
    correctCardinalities,
    correctAllCardinalities,
    findMaxMaxCardinality,
    findMaxMinCardinality,
    printableLayerWithDomainAndRange,
    printablePathFromTuplesWithDomainAndRange,
    RelationIndexNew,
    buildIndexDownFast',
    findSubBinaryRelationTreeFast',
    findSubBRelTreeWithCount,
    lookupSubBRelsOf,
    lookupSubBRelOf,
    validityFilter,
    cardinalityTestAllObjects,
    getTypeIdFromObject,
    cardinalityMetAllRels,
    printableErrorResults,
    filterOutErrorsBy,
    filterErrorsBy,
    cardinalityMet,
    rangeTestAllObjects,
    rangeMetTest,
    filterHigherLevelBrels,
    relationInSupertypePaths
  )
where

import qualified HqdmLib (
    Id,
    HqdmTriple(..),
    RelationPair,
    HqdmHasSupertype,
    getSubjects,
    getPredicates,
    uniqueIds,
    uniqueTriples,
    lookupHqdmOne,
    lookupHqdmType,
    lookupHqdmTypeIdFromName,
    lookupHqdmIdsFromTypePredicates,
    lookupSubtypes,
    lookupSubtypeOf,
    lookupSubtypesOf,
    lookupSupertypeOf,
    lookupSupertypesOf,
    findSupertypeTree,
    findHqdmTypesInList,
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
    deleteItemsFromList,
    lookupHqdmTypeFromAll,
    headIfUUIDPresent
    )

import GHC.Generics (Generic)
import Data.List ( isPrefixOf, sortOn )
import Data.Maybe ( fromJust )
import qualified Data.UUID (UUID, fromString, toString, null, nil)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- Following imports are for the upgrade to use Big-Endian Patricia Tree structure
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Csv ( FromField(..), ToField(..), FromRecord(..), ToRecord(..),
  Parser, record, toField, (.!) )

-- | In a RelationPairSet xR'y the  is a list of [R'y] for x, where R' can be any allowed 
--   number of instances of permitted Relations
data HqdmRelationSet = HqdmRelationSet
  { relationshipId :: !HqdmLib.Id,
    relationPairs :: ![HqdmLib.RelationPair]
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

-- | HqdmBinaryRelationPure is structured to be compatable with the original HQDM EXPRESS 
--   documentation (hqdm.exp) and the python extractor created to extract the following
--   properties of each relation specified in the EXPRESS data file.  A uuid is applied
--   to each specification for a binary relation in HQDM EXPRESS.  This should remain
--   fixed in the source hqdmRelations.csv.
--
-- Note: The has_supertype relations are not covered by this but can be added by hand.
--       This is because the EXPRESS notation handles super/sub-types as a separate 
--       keyword (e.g. "SUBTYPE OF")
--
--  data HqdmBinaryRelationPure:
--        domain = UUID           # Name of node that is the x in xR'y Binary Relation
--        relationId = RelationId # !RelationId,-- Relation unique Id (hqdmRel:uuid)
--        relationName = ""       # Name of Binary Relation Set (doesn't need to be unique?)
--        rangeSet = UUID         # Range Set id that is the y in xR'y ()
--        hasSuperBR = [UUID]     # SuperBR Set Id (empty if none?)
--        cardinalityMin = 0      # 0,1,...
--        cardinalityMax = -1     # -1 (no max!),0,1,2,...
--        redeclaredBR = False    # True means superBRtypes are abstract?
--        inverseOf = RelationId  # Inverse of named relation

type RelationId = Data.UUID.UUID
type RelationIndexNew = Map.Map RelationId (Set.Set RelationId)

-- | HqdmBinaryRelationPure
-- A data type that uses only identities to specify the xR'y of a HQDM Binary 
data HqdmBinaryRelationPure = HqdmBinaryRelationPure
  {
    pureDomain :: !HqdmLib.Id,            -- Must be present
    pureBinaryRelationId :: !RelationId,  -- Relation unique Id (hqdmRel:uuid)
    pureBinaryRelationName :: String,     -- Name of Binary Relation Set (doesn't need to be unique or even present)
    pureRange:: HqdmLib.Id,               -- Range Set ids.  Does this need to be a list? Not yet, however, can be empty as range can be out-of-model (i.e. a uuidv5 string hash)
    pureHasSuperBR :: [RelationId],       -- SuperBR Set Id (empty if none?)... Should be HqdmLib.Id
    pureCardinalityMin :: Int,            -- 0,1,...
    pureCardinalityMax :: Int,            -- -1 (indicates no max!),0,1,2,...
    pureRedeclaredBR :: Bool,             -- True means superBRtypes are abstract?
    pureInverseOf :: HqdmLib.Id
  }
  deriving (Show, Eq, Ord)

instance ToField Bool where
  toField True  = BC.pack "True"
  toField False = BC.pack "False"

instance FromField Bool where
  parseField s
    | str == "true"  = pure True
    | str == "false" = pure False
    | otherwise      = fail $ "Invalid Bool value: " ++ BC.unpack s
    where
      -- Convert to lowercase to make it case-insensitive and robust
      str = map Data.Char.toLower (BC.unpack s)

instance ToRecord HqdmBinaryRelationPure where
  toRecord r = record
    [ toField (pureDomain r)
    , toField (pureBinaryRelationId r)
    , toField (pureBinaryRelationName r)
    , toField (pureRange r)
    , toField (BC.pack $ unwords $ map Data.UUID.toString (pureHasSuperBR r)) -- Inline manual serialize
    , toField (pureCardinalityMin r)
    , toField (pureCardinalityMax r)
    , toField (pureRedeclaredBR r)
    , toField (pureInverseOf r)
    ]

instance FromRecord HqdmBinaryRelationPure where
  parseRecord v
    | length v /= 9 = fail "HqdmBinaryRelationPure requires exactly 9 fields"
    | otherwise = do
        pDomain     <- v .! 0
        pRelId      <- v .! 1
        pRelName    <- v .! 2
        pRange      <- v .! 3

        -- 1. Extract the raw delimited ByteString for the list field
        rawSuperBR  <- v .! 4 :: Parser BC.ByteString

        -- 2. Split and map the string parsing over the elements
        pSuperBR    <- if BC.null rawSuperBR
                         then pure []
                         else mapM parseUUIDField (BC.split ' ' rawSuperBR)

        pCardMin    <- v .! 5
        pCardMax    <- v .! 6
        pRedeclared <- v .! 7
        pInverseOf  <- if length v == 9
                         then do
                           rawInverse <- v .! 8 :: Parser BC.ByteString
                           if BC.null rawInverse
                             then pure Data.UUID.nil -- Or handle it as preferred
                             else parseUUIDField rawInverse
                         else pure Data.UUID.nil -- Fallback if column completely omitted

        pure $ HqdmBinaryRelationPure
          { pureDomain             = pDomain
          , pureBinaryRelationId   = pRelId
          , pureBinaryRelationName = pRelName
          , pureRange              = pRange
          , pureHasSuperBR         = pSuperBR
          , pureCardinalityMin     = pCardMin
          , pureCardinalityMax     = pCardMax
          , pureRedeclaredBR       = pRedeclared
          , pureInverseOf          = pInverseOf
          }

-- Helper parser to reuse inside the split loop
parseUUIDField :: BC.ByteString -> Parser Data.UUID.UUID
parseUUIDField bs = case Data.UUID.fromString (BC.unpack bs) of
  Just uuid -> pure uuid
  Nothing   -> fail $ "Invalid UUID in superBR list: " ++ BC.unpack bs

data HqdmBinaryRelationSet = HqdmBinaryRelationSet
  { nodeId :: !HqdmLib.Id,
    binaryRelationPairs :: [HqdmBinaryRelationPure]
  }
  deriving (Show, Eq, Generic)

data RelationCheck =
  Valid |
  Invalid |
  MaxCardinalityViolation |
  MinCardinalityViolation |
  RangeTypeViolation |
  RelationMissing |
  RelationInstanceNotPresent |
  UnexpectedRelation
  deriving (Eq, Ord, Enum, Show)

relationSetCheck:: RelationCheck -> HqdmBinaryRelationPure -> (RelationCheck, HqdmBinaryRelationPure)
relationSetCheck chk rel = (chk, rel)

relationSetAndIdCheck:: RelationCheck -> HqdmBinaryRelationPure -> HqdmLib.Id -> (RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)
relationSetAndIdCheck chk rel uid = (chk, rel, uid)

buildIndexDownFast' :: [HqdmBinaryRelationPure] -> RelationIndexNew
buildIndexDownFast' rels = Map.fromListWith Set.union 
  [ (superId, Set.singleton (pureBinaryRelationId r)) 
  | r <- rels
  , superId <- pureHasSuperBR r  
  ]

universalRelationSet::Data.UUID.UUID
universalRelationSet = fromJust $ Data.UUID.fromString "85e78ac0-ec72-478f-9aac-cacb520290a0"

hqdmAttributeBR::Data.UUID.UUID
hqdmAttributeBR = fromJust $ Data.UUID.fromString "69b0e5b9-3be2-4ec3-a9a6-bb5b523d4b32"

hqdmHasSupertypeId::Data.UUID.UUID
hqdmHasSupertypeId = fromJust $ Data.UUID.fromString "1f983e8a-7db1-4374-8fb1-7e8a432a967e"

hqdmHasSuperclassId::Data.UUID.UUID
hqdmHasSuperclassId = fromJust $ Data.UUID.fromString "7d11b956-0014-43be-9a3e-f89e2b31ec4f"

hqdmElementOfType::Data.UUID.UUID
hqdmElementOfType = fromJust $ Data.UUID.fromString "8130458f-ae96-4ab3-89b9-21f06a2aac78"

hqdmEntityName::Data.UUID.UUID
hqdmEntityName = fromJust $ Data.UUID.fromString "fe987366-a8ad-48fa-8821-73f54f6df180"

hqdmRecordCreated::Data.UUID.UUID
hqdmRecordCreated = fromJust $ Data.UUID.fromString "919a3f90-b681-422c-8481-fe313daa0044"

hqdmRecordCreator::Data.UUID.UUID
hqdmRecordCreator = fromJust $ Data.UUID.fromString "972bdd5f-5f8c-42d1-a47f-1ac08d1da48e"

getPureDomain :: HqdmBinaryRelationPure -> RelationId
getPureDomain = pureDomain

getPureRelationId :: HqdmBinaryRelationPure -> RelationId
getPureRelationId = pureBinaryRelationId

getPureRelationName :: HqdmBinaryRelationPure -> String
getPureRelationName = pureBinaryRelationName

getPureRange :: HqdmBinaryRelationPure -> RelationId
getPureRange = pureRange

getPureSuperRelation :: HqdmBinaryRelationPure -> [RelationId]
getPureSuperRelation = pureHasSuperBR

getPureSuperRelations :: [HqdmBinaryRelationPure] -> [RelationId]
getPureSuperRelations = concatMap pureHasSuperBR

getPureCardinalityMin :: HqdmBinaryRelationPure -> Int
getPureCardinalityMin = pureCardinalityMin

getPureCardinalityMax :: HqdmBinaryRelationPure -> Int
getPureCardinalityMax = pureCardinalityMax

getPureRedeclared :: HqdmBinaryRelationPure -> Bool
getPureRedeclared = pureRedeclaredBR

getPureInverseOf :: HqdmBinaryRelationPure -> RelationId
getPureInverseOf = pureInverseOf

printRelation :: HqdmBinaryRelationPure -> String
printRelation rel = "RELATION SPECIFICATION:\n\tDomain: " ++ Data.UUID.toString (getPureDomain rel) ++
  "\n\tRelation UUID: " ++ Data.UUID.toString (getPureRelationId rel) ++
  "\n\tOriginal Relation Name: " ++ getPureRelationName rel ++
  "\n\tRange: " ++ Data.UUID.toString (getPureRange rel) ++
  "\n\tMin Cardinality: " ++ show (getPureCardinalityMin rel) ++
  "\n\tMax Cardinality: " ++ show (getPureCardinalityMax rel) ++
  "\n\tInverse: " ++ Data.UUID.toString (getPureInverseOf rel) ++ "\n"


printRelationWithTypeNames :: HqdmBinaryRelationPure -> [HqdmLib.HqdmTriple] -> String
printRelationWithTypeNames rel tpls = "RELATION SPECIFICATION:\n\tDomain: " ++
  Data.UUID.toString (getPureDomain rel) ++ " type `" ++
  Data.UUID.toString (fromJust (HqdmLib.lookupHqdmType $ HqdmLib.lookupHqdmOne (getPureDomain rel) tpls)) ++ "'" ++
  "\n\tRelation UUID: " ++ Data.UUID.toString (getPureRelationId rel) ++
  "\n\tOriginal Relation Name: " ++ getPureRelationName rel ++
  "\n\tRange: " ++ Data.UUID.toString (getPureRange rel) ++ " type `" ++
  Data.UUID.toString (fromJust (HqdmLib.lookupHqdmType $ HqdmLib.lookupHqdmOne (getPureRange rel) tpls)) ++ "'" ++
  "\n\tMin Cardinality: " ++ show (getPureCardinalityMin rel) ++
  "\n\tMax Cardinality: " ++ show (getPureCardinalityMax rel) ++ "\n"


stringToBool :: String -> Bool
stringToBool x
  | x=="True" = True
  | otherwise = False

idListFromString :: String -> [String] -> [String]
idListFromString x lst
  | Prelude.null x = lst
  | length x == 36 = lst ++ [x]
  | length x >= 37 = idListFromString (drop 37 x) (lst ++ [take 36 x])

getRelationNameFromRels :: RelationId -> [HqdmBinaryRelationPure] -> String
getRelationNameFromRels relId brels = head ([pureBinaryRelationName values | values <- brels, relId == pureBinaryRelationId values])

getBrelDomainFromRels :: RelationId -> [HqdmBinaryRelationPure] -> HqdmLib.Id
getBrelDomainFromRels relId brels = fromJust $ HqdmLib.headIfUUIDPresent [pureDomain values | values <- brels, relId == pureBinaryRelationId values]

getBrelRangeFromRels :: RelationId -> [HqdmBinaryRelationPure] -> HqdmLib.Id
getBrelRangeFromRels relId brels = fromJust $ HqdmLib.headIfUUIDPresent [pureRange values | values <- brels, relId == pureBinaryRelationId values]

findBrelDomainSupertypes :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> [HqdmLib.Id]
findBrelDomainSupertypes relId brels = HqdmLib.lookupSupertypeOf (getBrelDomainFromRels relId brels)

findBrelFromId :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
findBrelFromId relId brels = take 1 [values | values <- brels, relId == pureBinaryRelationId values]

findBrelsFromDomain :: HqdmLib.Id -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
findBrelsFromDomain domId brels = [values | values <- brels, domId == pureDomain values]

findBrelsFromRange :: HqdmLib.Id -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
findBrelsFromRange rangeId brels = [values | values <- brels, rangeId == pureRange values]

findBrelsFromIds :: [RelationId] -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
findBrelsFromIds relIds brels = concatMap ( \ x -> take 1 [values | values <- brels, x == pureBinaryRelationId values] ) relIds

-- | superRelationPathsToUniversalRelation
-- From all the Binary Relations given find all the BR supertypes of a given RelationId
-- (supplied as a [[RelationId]]). This takes only has_supertype statements as [HqdmTriple].
-- The ouput is a list of layers from the supplied subtype to the termination empty layer
-- above thing.
superRelationPathsToUniversalRelation :: [[RelationId]] -> [HqdmBinaryRelationPure] -> [[RelationId]]
superRelationPathsToUniversalRelation relIds brels = go relIds brels
  where
    nextLayer = last relIds
    superBRs = getPureSuperRelations $ findBrelsFromIds nextLayer brels
    newLayer = [ HqdmLib.uniqueIds $ HqdmLib.deleteItemsFromList superBRs nextLayer]
    -- newLayer is formed from a defence against circularity.  Remove elements of newLayer that are in nextLayer.

    go relIds brels
      | Prelude.null newLayer = init relIds
      | newLayer == [[]] = relIds
      | sum [length $ filter (== universalRelationSet) yl | yl <- newLayer] > 0 = relIds ++ newLayer
      | otherwise = superRelationPathsToUniversalRelation (relIds ++ newLayer) brels

-- | findSubBRelTreeWithCount
-- From the provided Binary Relation Sets given, find the BRel Subsets for # layers in the count.
findSubBRelTreeWithCount :: [[RelationId]] -> [HqdmBinaryRelationPure] -> Int -> [[RelationId]]
findSubBRelTreeWithCount ids hqdmBrels cnt = go ids hqdmBrels cnt
  where
    nextLayer = last ids
    possibleNewLayer = HqdmLib.uniqueIds $ concat (lookupSubBRelsOf nextLayer hqdmBrels)
    newLayer = [HqdmLib.deleteItemsFromList possibleNewLayer (concat ids)]

    go ids brels cnt
      | cnt == 0 = ids
      | Prelude.null (head newLayer) = ids
      | otherwise = findSubBRelTreeWithCount (ids ++ newLayer) brels (subtract 1 cnt)

-- | relIdNameTupleLayers
-- Take a list of lists of Relation Ids, find their names and generate an equivalent list of Tuples of the Id and Name pairs
relIdNameTupleLayers :: [[RelationId]] -> [HqdmBinaryRelationPure] -> [[(RelationId, String)]]
relIdNameTupleLayers relIds brels = fmap (`relIdNameTuples` brels) relIds

-- | relIdNameTuples
-- Take a list of Relation Ids, find their names and generate a list of Tuples of the Id and Name pairs
relIdNameTuples :: [RelationId] -> [HqdmBinaryRelationPure] -> [(RelationId, String)]
relIdNameTuples relIds brels = fmap (\ x -> (x, head [pureBinaryRelationName values | values <- brels, x == pureBinaryRelationId values])) relIds

-- | printablePathFromTuples
-- Renders a layered path list of Tuples from the source to the destination as printable text.
printablePathFromTuples :: [[(RelationId, String)]] -> String
printablePathFromTuples tpls = concatMap (\ x -> printableLayer x ++ HqdmLib.fmtString "^\n" ++ HqdmLib.fmtString "/|\\\n" ++ HqdmLib.fmtString "|\n")  (reverse tpls)

-- | printablePathFromTuplesWithDomainAndRange
-- Renders a layered path list of Tuples from the source to the destination as printable text.
printablePathFromTuplesWithDomainAndRange :: [[(RelationId, String)]] -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> String
printablePathFromTuplesWithDomainAndRange tuples brels tpls  = reverse $ drop 303 (reverse $ concatMap (\ x -> printableLayerWithDomainAndRange x brels tpls ++ HqdmLib.fmtString "^\n" ++ HqdmLib.fmtString "/|\\\n" ++ HqdmLib.fmtString "|\n")  (reverse tuples))

getDomainName :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> String
getDomainName rid brels tpls = maybe "" Data.UUID.toString (HqdmLib.headIfUUIDPresent $ HqdmLib.findHqdmTypesInList [pureDomain $ head (findBrelFromId rid brels)] tpls)

getRangeName :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> String
getRangeName rid brels tpls = maybe "" Data.UUID.toString (HqdmLib.headIfUUIDPresent $ HqdmLib.findHqdmTypesInList [pureRange $ head (findBrelFromId rid brels)] tpls)

printableLayerWithDomainAndRange :: [(RelationId, String)] -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> String
printableLayerWithDomainAndRange tuples brels tpls =
  concatMap (\ x -> HqdmLib.fmtString ("[" ++ getDomainName (fst x) brels tpls ++ "] " ++ snd x ++ "(" ++ Data.UUID.toString (fst x) ++ ") [" ++ getRangeName (fst x) brels tpls ++ "]\n" )) tuples

printableLayer :: [(RelationId, String)] -> String
printableLayer = concatMap (\ x -> HqdmLib.fmtString (snd x ++ "," ++ Data.UUID.toString (fst x)) ++ "\n")

-- | This swaps the relation names in a HqdmAllAsData dataset (it doesn't handle instance and extended subclasses)
{-hqdmSwapTopRelationNamesForIds :: [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
hqdmSwapTopRelationNamesForIds hqdmTpls brels =
  fmap (`convertTopRelationByDomainAndName` brels) hqdmTpls-}

-- | This swaps the relation name in a HqdmAllAsData triple (it doesn't handle instance and extended subclass triples)
{-convertTopRelationByDomainAndName :: HqdmLib.HqdmTriple -> [HqdmBinaryRelationPure] -> HqdmLib.HqdmTriple
convertTopRelationByDomainAndName tpl brels = go tpl
  where
    pureRelMatch = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, (HqdmLib.subject tpl == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) ]
    hqdmTypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmType == pureBinaryRelationName values ]
    hqdmHasSupertypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSupertype == pureBinaryRelationName values ]
    hqdmHasSuperclassBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSuperclass == pureBinaryRelationName values ]

    go tpl
      | HqdmLib.predicate tpl==hqdmType = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSupertype = HqdmLib.HqdmTriple (HqdmLib.subject tpl)  hqdmHasSupertypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSuperclass  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasSuperclassBR (HqdmLib.object tpl)
      | otherwise = HqdmLib.HqdmTriple (HqdmLib.subject tpl) pureRelMatch (HqdmLib.object tpl)-}

{-convertRelationByDomainRangeAndName tpl brels = HqdmLib.HqdmTriple
  (HqdmLib.subject tpl)
  (HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, (HqdmLib.subject tpl == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) ])
  (HqdmLib.object tpl)-}

-- | This finds the domain type Id from an object Id and List of accopmanying triples
{-hqdmDomainTypeFromIdInList :: HqdmLib.HqdmTriple -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> HqdmLib.Id
hqdmDomainTypeFromIdInList tpl datasetTpls topTpls = go
  where
    typeOfSubject = HqdmLib.headIfStringPresent [ HqdmLib.object values | values <- datasetTpls, (hqdmType == HqdmLib.predicate values) && (HqdmLib.subject tpl ==  HqdmLib.subject values) ]

    go = HqdmLib.headIfStringPresent [ HqdmLib.subject values | values <- topTpls, (hqdmType == HqdmLib.predicate values) && (typeOfSubject ==  HqdmLib.object values) ]-}

-- | This finds the range type Id from an object Id and List of accopmanying triples
{-hqdmRangeTypeFromIdInList :: HqdmLib.HqdmTriple -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> HqdmLib.Id
hqdmRangeTypeFromIdInList tpl datasetTpls topTpls = go
  where
    typeOfObject = HqdmLib.headIfStringPresent [ HqdmLib.object values | values <- datasetTpls, (hqdmType == HqdmLib.predicate values) && (HqdmLib.object tpl ==  HqdmLib.subject values) ]

    go = HqdmLib.headIfStringPresent [ HqdmLib.subject values | values <- topTpls, (hqdmType == HqdmLib.predicate values) && (typeOfObject ==  HqdmLib.object values) ]-}

-- | This swaps the relation names in a HqdmAllAsData dataset (it doesn't handle instance and extended subclasses)
-- The first two lists should be the same input dataset(!?!?!)
{-hqdmSwapAnyRelationNamesForIds :: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple]-> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
hqdmSwapAnyRelationNamesForIds hqdmTpls topTpls brels = fmap (\ x -> convertAnyHqdmRelationByDomainAndName x (hqdmDomainTypeFromIdInList x hqdmTpls topTpls) brels) hqdmTpls-}

-- | This swaps the relation name in a HqdmAllAsData triple (it doesn't handle instance and extended subclass triples)
-- Takes the triple and the Top Type from which it inherits its relations
{-convertAnyHqdmRelationByDomainAndName :: HqdmLib.HqdmTriple -> HqdmLib.Id -> [HqdmBinaryRelationPure] -> HqdmLib.HqdmTriple
convertAnyHqdmRelationByDomainAndName tpl typeId brels = go tpl
  where
    pureRelMatch = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, (typeId == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) ]
    -- THESE PROPERTIES AND GUARDS SHOULD BE CONSIDERED TEMPORARY.  THEY CAN BE QUERIED FROM THE DATA BUT AN EXTRA GENERIC FUNCTION IS NEEDED FOR THIS
    hqdmTypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmType == pureBinaryRelationName values ]
    hqdmHasSupertypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSupertype == pureBinaryRelationName values ]
    hqdmHasSuperclassBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSuperclass == pureBinaryRelationName values ]
    hqdmHasElementOfTypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmElementOfType == pureBinaryRelationName values ]
    hqdmHasEntityNameBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmEntityName == pureBinaryRelationName values ]
    hqdmHasRecordCreatedBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmRecordCreated == pureBinaryRelationName values ]
    hqdmHasRecordCreatorBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmRecordCreator == pureBinaryRelationName values ]

    go tpl
      | HqdmLib.predicate tpl==hqdmType = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSupertype = HqdmLib.HqdmTriple (HqdmLib.subject tpl)  hqdmHasSupertypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSuperclass  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasSuperclassBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmElementOfType  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasElementOfTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmEntityName  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasEntityNameBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmRecordCreated  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasRecordCreatedBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmRecordCreator  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasRecordCreatorBR (HqdmLib.object tpl)
      | pureRelMatch == "" = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmAttributeBR (HqdmLib.object tpl)
      | otherwise = HqdmLib.HqdmTriple (HqdmLib.subject tpl) pureRelMatch (HqdmLib.object tpl)-}

-- | This swaps the relation names in a HqdmAllAsData dataset (it doesn't handle instance and extended subclasses)
-- Looks for match of domain type, relation name and range type
{-hqdmSwapAnyRelationNamesForIdsStrict :: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple]-> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
hqdmSwapAnyRelationNamesForIdsStrict hqdmTpls topTpls brels = fmap (\ x -> convertAnyHqdmRelationByDomainRangeAndName x (hqdmDomainTypeFromIdInList x hqdmTpls topTpls) (hqdmRangeTypeFromIdInList x hqdmTpls topTpls) brels topTpls) hqdmTpls-}

-- | This swaps the relation name in a HqdmAllAsData triple (it doesn't handle instance and extended subclass triples)
-- Takes the triple and the Top Type from which it inherits its relations
-- ** NOTE ** THIS HANDLES RANGE SUBTYPES
--    tpl           A triple to convert the predicate of
--    domainTypeId  The type of the subject (Left)
--    rangeTypeId   The type of the object (Right)
--    brels         The list of binary relation sets
--    topTpls       The full collection of hqdm supertype-subtype statements
{-convertAnyHqdmRelationByDomainRangeAndName :: HqdmLib.HqdmTriple -> HqdmLib.Id -> HqdmLib.Id -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmHasSupertype] -> HqdmLib.HqdmTriple
convertAnyHqdmRelationByDomainRangeAndName tpl domainTypeId rangeTypeId brels topTpls = go tpl
  where
    pureRelMatch = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, (domainTypeId == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) && (rangeTypeId == pureRange values) ]
    -- THESE PROPERTIES AND GUARDS SHOULD BE CONSIDERED TEMPORARY.  THEY CAN BE QUERIED FROM THE DATA BUT AN EXTRA GENERIC FUNCTION IS NEEDED FOR THIS
    subtypeRangeMatch = [ pureBinaryRelationId values | values <- brels, (domainTypeId == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) && (rangeTypeId `elem` concat (HqdmLib.findSubtypeTree [[pureRange values]] topTpls)) ]
    hqdmTypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmType == pureBinaryRelationName values ]
    hqdmHasSupertypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSupertype == pureBinaryRelationName values ]
    hqdmHasSuperclassBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSuperclass == pureBinaryRelationName values ]
    hqdmHasElementOfTypeBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmElementOfType == pureBinaryRelationName values ]
    hqdmHasEntityNameBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmEntityName == pureBinaryRelationName values ]
    hqdmHasRecordCreatedBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmRecordCreated == pureBinaryRelationName values ]
    hqdmHasRecordCreatorBR = HqdmLib.headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmRecordCreator == pureBinaryRelationName values ]

    go tpl
      | HqdmLib.predicate tpl==hqdmType = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSupertype = HqdmLib.HqdmTriple (HqdmLib.subject tpl)  hqdmHasSupertypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSuperclass  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasSuperclassBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmElementOfType  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasElementOfTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmEntityName  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasEntityNameBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmRecordCreated  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasRecordCreatedBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmRecordCreator  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasRecordCreatorBR (HqdmLib.object tpl)
      | not (Prelude.null subtypeRangeMatch) = HqdmLib.HqdmTriple (HqdmLib.subject tpl) ( getPureRelationId (head $ filterHigherLevelBrels (findBrelsFromIds subtypeRangeMatch brels) brels)) (HqdmLib.object tpl) -- This is for the case that there is a 
      | pureRelMatch == "" = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmAttributeBR (HqdmLib.object tpl)
      | otherwise = HqdmLib.HqdmTriple (HqdmLib.subject tpl) pureRelMatch (HqdmLib.object tpl)-}

-- | attributeStringsToUUIds

{- attributeStringsToUUIds :: HqdmLib.HqdmTriple -> HqdmLib.HqdmTriple
attributeStringsToUUIds x = if isDateTime (Just (HqdmLib.object x)) then HqdmLib.HqdmTriple (HqdmLib.subject x) (HqdmLib.predicate x) "uuid"
                          else x
  
  -- fmap (\x -> (maybe x (\y -> HqdmLib.HqdmTriple (HqdmLib.subject x) (HqdmLib.predicate x) (y)) ()) -- Need test for whether it is a data_EntityName predicate??
-}

-- | isSubtype
-- Boolean test to see if the first Type Id is a Subtype of the supplied second Type Id.  True if so.
isSubtype :: HqdmLib.Id -> HqdmLib.Id -> [HqdmLib.HqdmTriple] -> Bool
isSubtype id superTypeId tpls = id `elem` concat (HqdmLib.findSubtypeTree [[superTypeId]] tpls)

-- | subtypesOfFilter
-- Filter a list of given Type tuples to select only those that are subtypes of the given type Id
subtypesOfFilter :: [(HqdmLib.Id,HqdmLib.Id)] -> HqdmLib.Id -> [HqdmLib.HqdmTriple] -> [(HqdmLib.Id,HqdmLib.Id)]
subtypesOfFilter ids superTypeId tpls = filter (\ x -> isSubtype (snd x) superTypeId tpls) ids

-- | sortOnUuid
-- Sort a list of triples by the subject (uuid) field.
sortOnUuid :: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple]
sortOnUuid = sortOn HqdmLib.subject

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
      | Data.UUID.null relId = []
      | Prelude.null tpls = []
      | Prelude.null brels = []
      | otherwise = closestNameMatches


headListIfPresent :: [a] -> Maybe a
headListIfPresent []     = Nothing
headListIfPresent (a:as) = Just a

findSuperBinaryRelation' :: RelationId -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> Maybe (RelationId, String)
findSuperBinaryRelation' relId tpls brels =
  headListIfPresent [x | x <-
    findBrelsAndNamesWithDomains
      (findBrelDomainSupertypes relId brels (HqdmLib.lookupSubtypes tpls)) brels,
        snd x `isPrefixOf` getRelationNameFromRels relId brels]

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
  ( pureInverseOf x)

comma::String
comma = ","

boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

-- | printablePureRelation
-- Printable pure Relation for export as CSV
printablePureRelation :: HqdmBinaryRelationPure -> String
printablePureRelation x =
  Data.UUID.toString (pureDomain x) ++ comma ++
  Data.UUID.toString (pureBinaryRelationId x) ++ comma ++
  pureBinaryRelationName x ++ comma ++
  Data.UUID.toString (pureRange x) ++ comma ++
  concatMap (\x -> Data.UUID.toString x ++ " ") (pureHasSuperBR x) ++ comma ++
  show (pureCardinalityMin x) ++ comma ++
  show (pureCardinalityMax x) ++ comma ++
  boolToString (pureRedeclaredBR x) ++ comma ++
  Data.UUID.toString (pureInverseOf x) ++ "\n"

csvRelationsFromPure :: [HqdmBinaryRelationPure] -> String
csvRelationsFromPure = concatMap printablePureRelation

-- | lookupSuperBinaryRelsOf
-- From all the Pure Binary Relations given find the super Binary Relation of a given RelationId.
lookupSuperBinaryRelsOf :: RelationId -> [HqdmBinaryRelationPure] -> [RelationId]
lookupSuperBinaryRelsOf x brList = concat $ [ pureHasSuperBR values | values <- brList, x == pureBinaryRelationId values]

-- Go through a list of Binary Relations (typically a path to the universal Binary Relation) and find the maximum Minimum Cardinality value
findMaxMinCardinality :: [RelationId] -> [HqdmBinaryRelationPure] -> Int -> Int
findMaxMinCardinality _ [] cardVal = cardVal
findMaxMinCardinality [] _ cardVal = cardVal
findMaxMinCardinality (brelId:brelIds) brels cardVal = go brelIds brels cardVal
  where
    nextCardVal = pureCardinalityMin (head $ findBrelFromId brelId brels)

    go brelIds brels cardVal
      | Prelude.null brelIds = cardVal
      | nextCardVal > cardVal = findMaxMinCardinality brelIds brels nextCardVal
      | nextCardVal <= cardVal = findMaxMinCardinality brelIds brels cardVal
      | otherwise = -1

-- Go through a list of Binary Relations (typically a path to the universal Binary Relation) and find the maximum Maximum Cardinality value
findMaxMaxCardinality :: [RelationId] -> [HqdmBinaryRelationPure] -> Int -> Int
findMaxMaxCardinality _ [] cardVal = cardVal
findMaxMaxCardinality [] _ cardVal = cardVal
findMaxMaxCardinality (brelId:brelIds) brels cardVal = go brelIds brels cardVal
  where
    nextCardVal = pureCardinalityMax (head $ findBrelFromId brelId brels)

    go brelIds brels cardVal
      | Prelude.null brelIds = cardVal
      | nextCardVal > cardVal = findMaxMaxCardinality brelIds brels nextCardVal
      | nextCardVal <= cardVal = findMaxMaxCardinality brelIds brels cardVal
      | otherwise = -1

-- correctCardinalities
correctCardinalities :: HqdmBinaryRelationPure -> [HqdmBinaryRelationPure] -> HqdmBinaryRelationPure
correctCardinalities rel brels = newRel
  where
    superBRPathToUniversal = superRelationPathsToUniversalRelation [[pureBinaryRelationId rel]] brels
    maxMinCardinality = findMaxMinCardinality (concat superBRPathToUniversal) brels (pureCardinalityMin rel)
    maxMaxCardinality = findMaxMaxCardinality (concat superBRPathToUniversal) brels (pureCardinalityMax rel)

    newRel = addNewCardinalitiesToPure maxMinCardinality maxMaxCardinality rel

correctAllCardinalities :: [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
correctAllCardinalities brels = fmap (`correctCardinalities` brels) brels

----------------------------------------------------------------------------------------
-- | lookupSubBRelOf
-- From all the Binary Relation Sets that have the supplied relation id as a SuperBR
lookupSubBRelOf :: RelationId -> [HqdmBinaryRelationPure] -> [RelationId]
lookupSubBRelOf x list = [pureBinaryRelationId values | values <- list, x `elem` pureHasSuperBR values]

-- | lookupSubBRelsOf
-- Same as lookupSubBRelOf but takes a list of Ids and finds a list of Sub-Brels for each.
lookupSubBRelsOf :: [RelationId] -> [HqdmBinaryRelationPure] -> [[RelationId]]
lookupSubBRelsOf [] _ = []
lookupSubBRelsOf _ [] = []
lookupSubBRelsOf (id : ids) list = lookupSubBRelOf id list : lookupSubBRelsOf ids list

-- | findSubBinaryRelationTree
-- From all the BinaryRelations given by lookupSubBRels, find the subBrels of a given RelationId
-- Original pure function. Very slow
findSubBinaryRelationTree :: [[RelationId]] -> [HqdmBinaryRelationPure] -> [[RelationId]]
findSubBinaryRelationTree ids hqdmBrels = go ids hqdmBrels
  where
    nextLayer = last ids
    possibleNewLayer = HqdmLib.uniqueIds $ concat (lookupSubBRelsOf nextLayer hqdmBrels)
    newLayer = [HqdmLib.deleteItemsFromList possibleNewLayer (concat ids)]

    go ids hqdmBrel
      | Prelude.null (head newLayer) = ids
      | otherwise = findSubBinaryRelationTree (ids ++ newLayer) hqdmBrel

findSubBinaryRelationTreeFast' :: [[RelationId]] -> [HqdmBinaryRelationPure] -> [[RelationId]]
findSubBinaryRelationTreeFast' initialIds hqdmBrels =
    reverse (go initialLayers initialVisited (last initialLayers))
  where
    index = buildIndexDownFast' hqdmBrels

    initialLayers  = reverse initialIds
    initialVisited = Set.fromList (concat initialIds)

    go :: [[RelationId]] -> Set.Set RelationId -> [RelationId] -> [[RelationId]]
    go !accLayers !visited !currentLayer
      | null currentLayer = accLayers
      | Set.null filteredNewLayerSet = accLayers
      | otherwise = go (filteredNewLayer : accLayers) nextVisited filteredNewLayer
      where
        -- 1. Grab all child sets instantly and merge them into one unique Set
        allPossibleChildren = Set.unions [ Map.findWithDefault Set.empty pId index | pId <- currentLayer ]

        -- 2. Set difference instantly removes all visited items (replaces uniqueIds & filter)
        filteredNewLayerSet = Set.difference allPossibleChildren visited

        -- 3. Convert to list only once per layer for the final result layout
        filteredNewLayer    = Set.toList filteredNewLayerSet

        -- 4. Fast union to track visited nodes
        nextVisited         = Set.union visited filteredNewLayerSet

printableErrorResults:: [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> String
printableErrorResults errs hqdm tpls =
    concatMap (\ x ->
        "\n\nObject Id:" ++ show  (Data.UUID.toString (thdOf3 x)) ++ " of type '" ++ Data.UUID.toString (fromJust (HqdmLib.lookupHqdmType (HqdmLib.lookupHqdmOne (thdOf3 x) tpls))) ++ "'" ++
        "\nRelation check result: " ++ show (fstOf3 x) ++
        onlyPrintInvalidTypeCause x ++
        printRelationWithTypeNames (sndOf3 x) hqdm
        ) errs

onlyPrintInvalidTypeCause:: (RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id) -> String
onlyPrintInvalidTypeCause err
    | fstOf3 err == Valid = "\n"
    | fstOf3 err == RelationInstanceNotPresent = "\n"  -- If a necessary relation is not present it will separately raise a cardinality violation
    | otherwise = "\nLikely error, investigate to confirm it doesn't impact your use of the model and data.\n"

-- | filterOutErrorsBy x
-- Filter the list of cardinality results to remove the given super Brel Set
filterOutErrorsBy :: HqdmBinaryRelationPure -> [HqdmBinaryRelationPure] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
filterOutErrorsBy brel brels errs =
    [ values | values <- errs, not $ relationInSupertypePaths (getPureRelationId brel) [ sndOf3 values] brels False]

-- | filterErrorsBy x
-- Filter the list of cardinality results by to only include the given super Brel Set
filterErrorsBy :: HqdmBinaryRelationPure -> [HqdmBinaryRelationPure] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
filterErrorsBy brel brels errs =
    [ values | values <- errs, relationInSupertypePaths (getPureRelationId brel) [sndOf3 values] brels False]

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
            | Prelude.null brelSet = result
            | otherwise = relationInSupertypePaths relId (tail brelSet) brels (result || idInSuperBRels)

---------------------------------------------------------------------------------------------
-- Cardinality Check functions
---------------------------------------------------------------------------------------------

-- | validityFilter
-- Filters out Valid RelationCheck test results
validityFilter:: [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
validityFilter checkResults = [values | values <- checkResults,  not ((fstOf3 values == Valid) || (fstOf3 values == RelationInstanceNotPresent))]

-- | cardinalityTestAllObjects
-- Take the following arguments:
--    uuids   : The ids of the objects to test
--    tplsAll : The dataset that contains the objects to be tested
--    hqdm    : Hqdm AllAsData Types
--    brels   : Hqdm Binary Relation Sets
--    results : The list of aggregated results
cardinalityTestAllObjects:: [HqdmLib.Id] -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
cardinalityTestAllObjects uuids tplsAll hqdm brels results = go uuids tplsAll hqdm brels results
    where
        uuid = head uuids
        objTpls = HqdmLib.lookupHqdmOne uuid tplsAll
        typeId = getTypeIdFromObject objTpls hqdm
        typeBrels = findBrelsFromDomain typeId brels
        filteredBrels = filterHigherLevelBrels typeBrels brels

        go uuids tplsAll hqdm brels results
            | Prelude.null uuids = results
            | otherwise = cardinalityTestAllObjects (tail uuids) tplsAll hqdm brels (results ++ cardinalityMetAllRels objTpls filteredBrels)

-- | getTypeIdFromObject
-- Get the Id of the Hqdm Type from a supplied set of triples for a joined Hqdm object ### Implement test for this?
getTypeIdFromObject:: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> HqdmLib.Id
getTypeIdFromObject objTpls hqdm = head $ HqdmLib.lookupHqdmIdsFromTypePredicates hqdm (fromJust $ HqdmLib.lookupHqdmType objTpls)

-- | cardinalityMetAllRels
-- Tests whether the collection of triples for a single Hqdm Node (object) satisfies 
-- all of the HqdmBinaryRelationPure relation sets for that type of object. 
-- Take the following arguments:
--    tpls   : Triples for an object
--    brels  : Binary relation sets expected for this [HQDM] type of object
cardinalityMetAllRels:: [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
cardinalityMetAllRels _ [] = []
cardinalityMetAllRels [] _ = []
cardinalityMetAllRels tpls (brel : brels) = relationSetAndIdCheck (cardinalityMet tpls brel) brel (HqdmLib.subject $ head tpls) : cardinalityMetAllRels tpls brels

-- | cardinalityMet
-- Tests whether the collection of triples for a single Hqdm Node (object) satisfies the cardinality constraints
-- supplied HqdmBinaryRelationPureheadIfStringPresent tpls
-- Take the following arguments:
--    tpls   : Triples for an object
--    brel  : Binary relation set for checking object with
cardinalityMet:: [HqdmLib.HqdmTriple] -> HqdmBinaryRelationPure -> RelationCheck
cardinalityMet tpls brel = go
    where
        brelId = getPureRelationId brel
        brelCMin = getPureCardinalityMin brel
        brelCMax = getPureCardinalityMax brel
        relCmp = fmap (\ x -> (oneOrZero (brelId == HqdmLib.predicate x)::Int)) tpls
        relCount = sum relCmp

        go
            | (relCount > brelCMax) && (brelCMax > 0) = MaxCardinalityViolation
            | (relCount < brelCMin) && (brelCMin > 0) = MinCardinalityViolation
            | otherwise = Valid

---------------------------------------------------------------------------------------------
-- Range check functions
---------------------------------------------------------------------------------------------
-- | rangeTestAllObjects
-- Take the following arguments:
--    uuids   : The ids of the objects to test
--    tplsAll : The dataset that contains the objects to be tested
--    hqdm    : Hqdm AllAsData Types
--    brels   : Hqdm Binary Relation Sets
--    results : The list of aggregated results
rangeTestAllObjects:: [HqdmLib.Id] -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
rangeTestAllObjects uuids tplsAll hqdm brels results = go uuids tplsAll hqdm brels results
    where
        uuid = head uuids
        objTpls = HqdmLib.lookupHqdmOne uuid tplsAll
        objTypeId = getTypeIdFromObject objTpls hqdm
        typeBrels = findBrelsFromDomain objTypeId brels
        filteredBrels = filterHigherLevelBrels typeBrels brels

        go uuids tplsAll hqdm brels results
            | Prelude.null uuids = results
            | otherwise = rangeTestAllObjects (tail uuids) tplsAll hqdm brels (results ++ rangeMetAllRels objTpls tplsAll hqdm filteredBrels)

-- | rangeMetAllRels
-- Tests whether the collection of triples for a single Hqdm Node (object) satisfies the range type constraints
-- all of the HqdmBinaryRelationPure relation sets for that type of object. 
-- Take the following arguments:
--    tpls   : Triples for an object
--    tplsAll: The dataset that contains the objects to be tested
--    hqdm   : Hqdm AllAsData Types
--    brels  : Binary relation sets expected for this [HQDM] type of object
rangeMetAllRels:: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [(RelationCheck, HqdmBinaryRelationPure, HqdmLib.Id)]
rangeMetAllRels _ _ _ [] = []
rangeMetAllRels _ _ [] _ = []
rangeMetAllRels _ [] _ _ = []
rangeMetAllRels [] _ _ _ = []
rangeMetAllRels tpls tplsAll hqdm (brel : brels) = relationSetAndIdCheck (rangeMet tpls tplsAll hqdm brel) brel (HqdmLib.subject $ head tpls) : rangeMetAllRels tpls tplsAll hqdm brels

-- | rangeMet
-- Tests whether the collection of triples for a single Hqdm Node (object) satisfies the
-- supplied HqdmBinaryRelationPureheadIfStringPresent tpls
-- Take the following arguments:
--    tpls   : Triples for an object
--    tplsAll: The dataset that contains the objects to be tested
--    hqdm   : Triples for the HQDM AllAsData Types
--    brel   : Binary relation set for checking the range of this object's relationships that are of this BRel Set
rangeMet:: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> HqdmBinaryRelationPure -> RelationCheck
rangeMet tpls tplsAll hqdm brel = go
    where
        brelRange = getPureRange brel
        rangeInstanceOfBrel = fromJust $ HqdmLib.headIfUUIDPresent [HqdmLib.object values | values <- tpls, getPureRelationId brel == HqdmLib.predicate values]
        tplsForRangeObject = HqdmLib.lookupHqdmOne rangeInstanceOfBrel tplsAll
        typeOfInstanceOfBrel = fromJust $ HqdmLib.headIfUUIDPresent (HqdmLib.lookupHqdmTypeFromAll tplsForRangeObject rangeInstanceOfBrel) --- Maybe need to lookupHqdmOne here
        idOfType = getTypeIdFromObject hqdm tplsForRangeObject
        subTypeTreeOfRange = concat $ HqdmLib.findSubtypeTree [[brelRange]] hqdm

        go
            | Data.UUID.null rangeInstanceOfBrel = RelationInstanceNotPresent
            | idOfType `elem` subTypeTreeOfRange = Valid
            | otherwise = RangeTypeViolation

data RelationCheckTest = RelationCheckTest
  { brelRange :: String,
    rangeInstanceOfBrel :: String,
    typeOfInstanceOfBrel :: String,
    idOfType :: String,
    tpls :: [HqdmLib.HqdmTriple]
   } deriving (Eq, Show, Generic)

rangeMetTest:: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> HqdmBinaryRelationPure -> RelationCheckTest
rangeMetTest tpls tplsAll hqdm brel = go tpls
    where
        brelRange = getPureRange brel
        rangeInstanceOfBrel = fromJust $ HqdmLib.headIfUUIDPresent [HqdmLib.object values | values <- tpls, getPureRelationId brel == HqdmLib.predicate values]
        tplsForRangeObject = HqdmLib.lookupHqdmOne rangeInstanceOfBrel tplsAll
        typeOfInstanceOfBrel = fromJust $ HqdmLib.headIfUUIDPresent (HqdmLib.lookupHqdmTypeFromAll tplsForRangeObject rangeInstanceOfBrel) --- Maybe need to lookupHqdmOne here
        idOfType = fromJust $ HqdmLib.lookupHqdmTypeIdFromName hqdm typeOfInstanceOfBrel
        subTypeTreeOfRange = concat $ HqdmLib.findSubtypeTree [[brelRange]] hqdm

        go tpls
            | Data.UUID.null rangeInstanceOfBrel = RelationCheckTest  (Data.UUID.toString brelRange) "No range"  (Data.UUID.toString typeOfInstanceOfBrel)  (Data.UUID.toString idOfType) tpls -- Nugatory line now. FIX!
            | otherwise = RelationCheckTest  (Data.UUID.toString brelRange)  (Data.UUID.toString rangeInstanceOfBrel)  (Data.UUID.toString typeOfInstanceOfBrel)  (Data.UUID.toString idOfType) tpls

---------------------------------------------------------------------------------------------
-- Useful functions used in those above
---------------------------------------------------------------------------------------------

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