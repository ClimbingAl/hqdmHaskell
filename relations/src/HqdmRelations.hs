{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
    hqdmSwapAnyRelationNamesForIds,
    printableLayerWithDomainAndRange,
    printablePathFromTuplesWithDomainAndRange
  )
where

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
    fmtString,
    deleteItemsFromList
    )

import GHC.Generics (Generic)
import Data.Csv (FromRecord)
import Data.List (isPrefixOf, sortOn)

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
    pureHasSuperBR :: [RelationId],       -- SuperBR Set Id (empty if none?)... Should be HqdmLib.Id
    pureCardinalityMin :: Int,            -- 0,1,...
    pureCardinalityMax :: Int,            -- -1 (indicates no max!),0,1,2,...
    pureRedeclaredBR :: Bool,             -- True means superBRtypes are abstract?
    pureRedeclaredFromRange :: HqdmLib.Id
  }
  deriving (Show, Eq, Generic)

universalRelationSet::String
universalRelationSet = "85e78ac0-ec72-478f-9aac-cacb520290a0"

hqdmAttributeBR::String
hqdmAttributeBR = "69b0e5b9-3be2-4ec3-a9a6-bb5b523d4b32"

hqdmType::String
hqdmType = "type"

hqdmHasSupertype::String
hqdmHasSupertype = "has_supertype"

hqdmHasSuperclass::String
hqdmHasSuperclass = "has_superclass"

hqdmElementOfType::String
hqdmElementOfType = "element_of_type"

hqdmEntityName::String
hqdmEntityName = "data_EntityName"

hqdmRecordCreated::String
hqdmRecordCreated = "record_created"

hqdmRecordCreator::String
hqdmRecordCreator = "record_creator"

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

getPureRedeclaredFromRange :: HqdmBinaryRelationPure -> RelationId
getPureRedeclaredFromRange = pureRedeclaredFromRange

printRelation :: HqdmBinaryRelationPure -> String
printRelation rel = "RELATION SPECIFICATION:\n\tDomain: " ++ getPureDomain rel ++ 
  "\n\tRelation UUID: " ++ getPureRelationId rel ++ 
  "\n\tOriginal Relation Name: " ++ getPureRelationName rel ++ 
  "\n\tRange: " ++ getPureRange rel ++
  "\n\tMin Cardinality: " ++ show (getPureCardinalityMin rel) ++
  "\n\tMax Cardinality: " ++ show (getPureCardinalityMax rel) ++ "\n"

stringToBool :: String -> Bool
stringToBool x
  | x=="True" = True
  | otherwise = False

headIfStringPresent :: [String] -> String
headIfStringPresent x
  | not (null x)   = head x
  | otherwise      = ""

idListFromString :: String -> [String] -> [String]
idListFromString x lst
  | null x = lst
  | length x == 36 = lst ++ [x]
  | length x >= 37 = idListFromString (drop 37 x) (lst ++ [take 36 x])

hqdmRelationsToPure :: [HqdmBinaryRelation] -> [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure]
hqdmRelationsToPure brels tpls = fmap ( \ x -> HqdmBinaryRelationPure
  ( headIfStringPresent (HqdmLib.lookupHqdmIdFromType tpls (domain x) ) )
  ( binaryRelationId x)
  ( binaryRelationName x)
  ( range x  )
  ( idListFromString (hasSuperBR x) [] )
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
  ( idListFromString (hasSuperBR x) []  )
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

findBrelsFromDomain :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmBinaryRelationPure]
findBrelsFromDomain domId brels = [values | values <- brels, domId == pureDomain values]

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
      | null newLayer = init relIds
      | newLayer == [[]] = relIds
      | sum [length $ filter (== universalRelationSet) yl | yl <- newLayer] > 0 = relIds ++ newLayer
      | otherwise = superRelationPathsToUniversalRelation (relIds ++ newLayer) brels

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
getDomainName rid brels tpls = headIfStringPresent $ HqdmLib.findHqdmTypesInList [pureDomain $ head (findBrelFromId rid brels)] tpls

getRangeName :: RelationId -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> String
getRangeName rid brels tpls = headIfStringPresent $ HqdmLib.findHqdmTypesInList [pureRange $ head (findBrelFromId rid brels)] tpls

printableLayerWithDomainAndRange :: [(RelationId, String)] -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple] -> String
printableLayerWithDomainAndRange tuples brels tpls =
  concatMap (\ x -> HqdmLib.fmtString ("[" ++ getDomainName (fst x) brels tpls ++ "] " ++ snd x ++ "(" ++ fst x ++ ") [" ++ getRangeName (fst x) brels tpls ++ "]\n" )) tuples

printableLayer :: [(RelationId, String)] -> String
printableLayer = concatMap (\ x -> HqdmLib.fmtString (snd x ++ "," ++ fst x) ++ "\n")

-- | This swaps the relation names in a HqdmAllAsData dataset (it doesn't handle instance and extended subclasses)
hqdmSwapTopRelationNamesForIds :: [HqdmLib.HqdmTriple] -> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
hqdmSwapTopRelationNamesForIds hqdmTpls brels =
  fmap (`convertTopRelationByDomainAndName` brels) hqdmTpls

-- | This swaps the relation name in a HqdmAllAsData triple (it doesn't handle instance and extended subclass triples)
convertTopRelationByDomainAndName :: HqdmLib.HqdmTriple -> [HqdmBinaryRelationPure] -> HqdmLib.HqdmTriple
convertTopRelationByDomainAndName tpl brels = go tpl
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

-- | This finds the domain Id from an object Id and List of accopmanying triples
hqdmDomainTypeFromIdInList :: HqdmLib.HqdmTriple -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> HqdmLib.Id
hqdmDomainTypeFromIdInList tpl datasetTpls topTpls = go tpl
  where
    typeOfSubject = headIfStringPresent [ HqdmLib.object values | values <- datasetTpls, (hqdmType == HqdmLib.predicate values) && (HqdmLib.subject tpl ==  HqdmLib.subject values) ]

    go tpl = headIfStringPresent [ HqdmLib.subject values | values <- topTpls, (hqdmType == HqdmLib.predicate values) && (typeOfSubject ==  HqdmLib.object values) ]

-- | This swaps the relation names in a HqdmAllAsData dataset (it doesn't handle instance and extended subclasses)
-- The first two lists should be the same input dataset(!?!?!)
hqdmSwapAnyRelationNamesForIds :: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple]-> [HqdmBinaryRelationPure] -> [HqdmLib.HqdmTriple]
hqdmSwapAnyRelationNamesForIds hqdmTpls topTpls brels = fmap (\ x -> convertAnyHqdmRelationByDomainAndName x (hqdmDomainTypeFromIdInList x hqdmTpls topTpls) brels) hqdmTpls

-- | This swaps the relation name in a HqdmAllAsData triple (it doesn't handle instance and extended subclass triples)
-- Takes the triple and the Top Type from which it inherits its relations
convertAnyHqdmRelationByDomainAndName :: HqdmLib.HqdmTriple -> HqdmLib.Id -> [HqdmBinaryRelationPure] -> HqdmLib.HqdmTriple
convertAnyHqdmRelationByDomainAndName tpl typeId brels = go tpl
  where
    pureRelMatch = headIfStringPresent [ pureBinaryRelationId values | values <- brels, (typeId == pureDomain values) && (HqdmLib.predicate tpl ==  pureBinaryRelationName values) ]
    -- THESE PROPERTIES AND GUARDS SHOULD BE CONSIDERED TEMPORARY.  THEY CAN BE QUERIED FROM THE DATA BUT AN EXTRA GENERIC FUNCTION IS NEEDED FOR THIS
    hqdmTypeBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmType == pureBinaryRelationName values ]
    hqdmHasSupertypeBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSupertype == pureBinaryRelationName values ]
    hqdmHasSuperclassBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmHasSuperclass == pureBinaryRelationName values ]
    hqdmHasElementOfTypeBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmElementOfType == pureBinaryRelationName values ]
    hqdmHasEntityNameBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmEntityName == pureBinaryRelationName values ]
    hqdmHasRecordCreatedBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmRecordCreated == pureBinaryRelationName values ]
    hqdmHasRecordCreatorBR = headIfStringPresent [ pureBinaryRelationId values | values <- brels, hqdmRecordCreator == pureBinaryRelationName values ]

    go tpl
      | HqdmLib.predicate tpl==hqdmType = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSupertype = HqdmLib.HqdmTriple (HqdmLib.subject tpl)  hqdmHasSupertypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmHasSuperclass  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasSuperclassBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmElementOfType  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasElementOfTypeBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmEntityName  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasEntityNameBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmRecordCreated  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasRecordCreatedBR (HqdmLib.object tpl)
      | HqdmLib.predicate tpl==hqdmRecordCreator  = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmHasRecordCreatorBR (HqdmLib.object tpl)
      | pureRelMatch == "" = HqdmLib.HqdmTriple (HqdmLib.subject tpl) hqdmAttributeBR (HqdmLib.object tpl)
      | otherwise = HqdmLib.HqdmTriple (HqdmLib.subject tpl) pureRelMatch (HqdmLib.object tpl)

-- | isSubtype
-- Boolean test to see if the first Type Id is a Subtype of the supplied second Type Id.  True if so.
isSubtype :: HqdmLib.Id -> HqdmLib.Id -> [HqdmLib.HqdmTriple] -> Bool
isSubtype id superTypeId tpls = id `elem` concat (HqdmLib.findSubtypeTree [[superTypeId]] tpls [])

-- | subtypesOfFilter
-- Filter a list of given Type tuples to select onlt those that are subtypes of the given type Id
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

{-addStRelationToPure :: Maybe (RelationId, String) -> HqdmBinaryRelationPure -> HqdmBinaryRelationPure
addStRelationToPure stRel x = HqdmBinaryRelationPure
  ( pureDomain x)
  ( pureBinaryRelationId x)
  ( pureBinaryRelationName x)
  ( pureRange x)
  ( getRelationIdFromMonadTuple stRel )
  ( pureCardinalityMin x)
  ( pureCardinalityMax x)
  ( pureRedeclaredBR x)
  ( pureRedeclaredFromRange x)-}

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
  concatMap (++ " ") (pureHasSuperBR x) ++ comma ++
  show (pureCardinalityMin x) ++ comma ++
  show (pureCardinalityMax x) ++ comma ++
  boolToString (pureRedeclaredBR x) ++ comma ++
  pureRedeclaredFromRange x ++ "\n"

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
      | null brelIds = cardVal
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
      | null brelIds = cardVal
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