{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  HqdmLib
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

module HqdmLib
  ( Id,
    HqdmTriple(..),
    RelationPair(..),
    HqdmHasSupertype,
    FromRecord,
    ToRecord,
    headIfUUIDPresent,
    lastIfUUIDPresent,
    getSubjects,
    getPredicates,
    uniqueIds,
    uniqueTriples,
    uuidListSort,
    lookupHqdmOne,
    lookupHqdmName,
    lookupAllRelsToId,
    lookupHqdmTypeIdFromName,
    lookupHqdmIdsFromTypePredicates,
    lookupHqdmTypeNameFromId,
    relationPairs,
    lookupHqdmType,
    lookupHqdmTypeFromAll,
    lookupSubtypes,
    lookupSubtypeOf,
    lookupSubtypesOf,
    lookupSupertypeOf,
    lookupSupertypesOf,
    findHqdmTypeNamesInList,
    findHqdmNamesInList,
    findSupertypeTree,
    printableTypeTree,
    findSubtypeTree,
    findInheritedRels,
    findInheritedRels',
    collapseInheritedRels,
    printableCollapsedList,
    printableRelationPairs,
    printableRelationPairList,
    exportAsTriples,
    csvTriplesFromHqdmTriples,
    screenCharOffset,
    fmtString,
    deleteItemsFromList,
    nodeIdentityTest
  )
where

import Data.Csv (FromRecord, ToRecord, ToField, FromField, parseField, toField)
import qualified Data.ByteString.Char8 as BC
import Data.List (elemIndices)
import GHC.Generics (Generic)
import HqdmIds (thing)
import Data.UUID (UUID, fromString, toString)
import qualified Data.UUID as UUID

import Data.Maybe (fromJust)

data HqdmTriple = HqdmTriple
  { subject :: !Id,
    predicate :: !Id,
    object :: !Id
  }
  deriving (Show, Eq, Generic)

instance FromRecord HqdmTriple
instance ToRecord HqdmTriple

instance ToField UUID where
    toField = UUID.toASCIIBytes

-- | Parse a UUID from a CSV field (ByteString)
{-instance FromField UUID where
    parseField bs = case UUID.fromASCIIBytes bs of
        Just uuid -> pure uuid
        Nothing   -> fail "Invalid UUID format in CSV field"-}

instance FromField UUID where
    parseField s = case fromString (BC.unpack s) of
        Just uuid -> pure uuid
        Nothing   -> fail $ "Invalid UUID string: " ++ BC.unpack s

data RelationPair = RelationPair
  { p :: !Id,
    o :: !Id
  }
  deriving (Show, Eq, Generic)

-- type synonyms to handle the CSV contents
type Id = UUID

type Object = UUID

type Predicate = UUID

type HqdmHasSupertype = HqdmTriple

-- Names of relations relevant to importing triples

hqdmTypeId::UUID
hqdmTypeId = fromJust $ fromString "7e249a64-9f13-47d3-a232-562a3d080198"

hqdmHasSupertypeId::UUID
hqdmHasSupertypeId = fromJust $ fromString "1f983e8a-7db1-4374-8fb1-7e8a432a967e"

hqdmHasSuperclassId::UUID
hqdmHasSuperclassId = fromJust $ fromString "7d11b956-0014-43be-9a3e-f89e2b31ec4f"

hqdmDataEntityName::UUID
hqdmDataEntityName = fromJust $ fromString "fe987366-a8ad-48fa-8821-73f54f6df180"

hqdmBeginningId::UUID
hqdmBeginningId = fromJust $ fromString "96c965a9-ec3e-47f2-b18e-b67147bc0873"

screenCharOffset :: Int
screenCharOffset = 100

-- | nodeIdentityTest
-- Check that an element (subject or object) is a Node Id.  Done without regex to avoid dependencies.
-- Expects to be True for a UUID
nodeIdentityTest :: String -> Bool
nodeIdentityTest x = length x == 36 && ('-' `elemIndices` x) == [8, 13, 18, 23]

getSubjects :: [HqdmTriple] -> [Id]
getSubjects = map subject

getPredicates :: [HqdmTriple] -> [Predicate]
getPredicates = map predicate

getObjects :: [HqdmTriple] -> [Object]
getObjects xs = map object xs -- Can't Eta reduce due to "object" name collision

uniqueIds :: [Id] -> [Id]
uniqueIds xs = [x | (x, y) <- zip xs [0 ..], x `notElem` take y xs]

uniqueTriples :: [HqdmTriple] -> [HqdmTriple]
uniqueTriples xs = [x | (x, y) <- zip xs [0 ..], x `notElem` take y xs]

-----------------------------------------------------------------------------------
-- Utils to move to another Lib
-----------------------------------------------------------------------------------
headIfUUIDPresent :: [UUID] -> Maybe UUID
headIfUUIDPresent x
  | not (null x)   = Just (head x)
  | otherwise      = Nothing

lastIfUUIDPresent :: [UUID] -> Maybe UUID
lastIfUUIDPresent x
  | not (null x)   = Just (last x)
  | otherwise      = Nothing

-------------------------------------------------
-- Based on an online source (not covered by Copyright):
-- function declaration for function insert
insert :: [UUID] -> UUID -> [UUID]
-- function definition for function insert
-- base case
insert [] y = [y]
insert (x : xs) y =
  if y < x
    then [y] ++ [x] ++ xs
    else x : insert xs y

-- function declaration for function insert
uuidListSort :: [UUID] -> [UUID]
uuidListSort [] = []
uuidListSort (x : xs) = insert (uuidListSort xs) x

deleteItemsFromList :: [UUID] -> [UUID] -> [UUID]
deleteItemsFromList fromList itemsToRemove = [x | x <- fromList, x `notElem` itemsToRemove]

-------------------------------------------------------------------------------------
-- End of Utils
-------------------------------------------------------------------------------------

-------------------------------------------------
-- GRAPH FUNCTIONS USING THE LIST OF TRIPLES OF HqdmAllAsData

-- | lookupHqdmOne
-- Find all the triples that have the given node Id (subject).
lookupHqdmOne :: Id -> [HqdmTriple] -> [HqdmTriple]
lookupHqdmOne x tpls = [values | values <- tpls, x == subject values]

-- | lookupAllRelsToId
-- Find all the triples that have the given node Id in the object position.
-- i.e. Obtain all relationships to the given node Id. 
lookupAllRelsToId :: Id -> [HqdmTriple] -> [HqdmTriple]
lookupAllRelsToId x tpls = [values | values <- tpls, x == object values]

-- | relationPairs
-- Take a list of triples (typically with the same node Id from lookupHqdmOne) and return a list of the relation pairs.
relationPairs :: [HqdmTriple] -> [RelationPair]
relationPairs = fmap (\x -> RelationPair (predicate x) (object x))

-- | lookupHqdmTypeFromAll
-- From the complete set of HQDM triples with a given node Id (subject), from lookupHqdmOne, find the type name of the given Node Id.
lookupHqdmTypeFromAll :: [HqdmTriple] -> UUID -> [UUID]
lookupHqdmTypeFromAll hqdmAll nodeId = [object values | values <- hqdmAll, (hqdmTypeId == predicate values) && (nodeId == subject values)]

-- | lookupHqdmTypeIdFromName
-- From a set of HQDM AllAsData triples, find the ObjectId from a given Type Name
lookupHqdmTypeIdFromName :: [HqdmTriple] -> UUID -> Maybe UUID
lookupHqdmTypeIdFromName hqdmAll typeName = headIfUUIDPresent [subject values | values <- hqdmAll, (hqdmTypeId == predicate values) && (typeName == object values)]

lookupHqdmTypeNameFromId :: [HqdmTriple] -> UUID -> Maybe UUID
lookupHqdmTypeNameFromId hqdmAll typeId = headIfUUIDPresent [object values | values <- hqdmAll, (hqdmTypeId == predicate values) && (typeId == subject values)]

-- | lookupHqdmType
-- From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate type.
lookupHqdmType :: [HqdmTriple] -> Maybe UUID
lookupHqdmType obj = headIfUUIDPresent ([object values | values <- obj, hqdmTypeId == predicate values])

-- | lookupHqdmName
-- From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate type.
lookupHqdmName :: [HqdmTriple] -> Maybe UUID
lookupHqdmName obj = headIfUUIDPresent $ [object values | values <- obj, hqdmDataEntityName == predicate values]

-- | lookupHqdmIdsFromTypePredicates
-- From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate type.
lookupHqdmIdsFromTypePredicates :: [HqdmTriple] -> UUID -> [Id]
lookupHqdmIdsFromTypePredicates objs typeName = [subject values | values <- objs, (hqdmTypeId == predicate values) && (typeName == object values)]


toStringForced :: Maybe UUID -> String
toStringForced id = toString ( fromJust id )

-- | findHqdmTypeNamesInList
-- Find the type names of each given node Id (subject).
findHqdmTypeNamesInList :: [Id] -> [HqdmTriple] -> [String]
findHqdmTypeNamesInList ids hqdmModel = fmap (\ x -> toStringForced $ lookupHqdmType $ lookupHqdmOne x hqdmModel) ids

-- | findHqdmNamesInList
-- Find the data entity names of each given node Id (subject).
findHqdmNamesInList :: [Id] -> [HqdmTriple] -> [String]
findHqdmNamesInList ids hqdmModel = fmap (\ x -> toStringForced $ lookupHqdmName $ lookupHqdmOne x hqdmModel) ids

-- | lookupSubtypes
-- From all the triples that have the has_supertype or has_superclass predicate.
lookupSubtypes :: [HqdmTriple] -> [HqdmHasSupertype]
lookupSubtypes list =
  [
    values
    | values <- list,
      (hqdmHasSupertypeId == predicate values) || (hqdmHasSuperclassId == predicate values)
  ]

-- | lookupSubtypeOf
-- From all the triples given by lookupSubtypes find the subtypes of a given node Id.
-- This takes only has_supertype statements as [HqdmTriple]
lookupSubtypeOf :: Id -> [HqdmHasSupertype] -> [Id]
lookupSubtypeOf x list = [subject values | values <- list, x == object values]  -- PERHAPS ADD PREDICATE CHECK IN CASE HqdmHasSupertype LIST CONTAINS OTHER BRELS

-- | lookupSubtypesOf
-- Same as lookupSubtypeOf but takes a list of Ids and finds a list of subtypes for each.
lookupSubtypesOf :: [Id] -> [HqdmHasSupertype] -> [[Id]]
lookupSubtypesOf [] _ = []
lookupSubtypesOf _ [] = []
lookupSubtypesOf (id : ids) list = lookupSubtypeOf id list : lookupSubtypesOf ids list

-- | lookupSupertypOf
-- From all the triples given by lookupSubtypes find the supertypes of a given node Id.
-- This takes only has_supertype statements as [HqdmTriple]
lookupSupertypeOf :: Id -> [HqdmHasSupertype] -> [Id]
lookupSupertypeOf x list = [object values | values <- list, x == subject values && ((hqdmHasSupertypeId == predicate values) || (hqdmHasSuperclassId == predicate values))]

-- | lookupSupertypesOf
-- Same as lookupSupertypeOf but takes a list of Ids and finds a list of supertypes for each.
lookupSupertypesOf :: [Id] -> [HqdmHasSupertype] -> [[Id]]
lookupSupertypesOf [] _ = []
lookupSupertypesOf _ [] = []
lookupSupertypesOf (id : ids) list = lookupSupertypeOf id list : lookupSupertypesOf ids list

-- | findSupertypeTree
-- From all the triples given by lookupSupertypes find all the supertypes of a given node Id
-- (supplied as a [[id]]). This takes only has_supertype statements as [HqdmTriple].
-- The ouput is a list of layers from the supplied subtype to the termination empty layer
-- above thing.
findSupertypeTree :: [[Id]] -> [HqdmHasSupertype] -> [[Id]]
findSupertypeTree ids hqdm = go ids hqdm
  where
    nextLayer = last ids
    possibleNewLayer = uniqueIds $ concat (lookupSupertypesOf nextLayer hqdm)
    newLayer = [deleteItemsFromList possibleNewLayer (take 1 nextLayer)]

    go ids hqdm
      | null newLayer = init ids
      | newLayer == [[]] = ids
      | sum [length $ filter (== thing) yl | yl <- newLayer] > 0 = ids ++ newLayer
      | otherwise = findSupertypeTree (ids ++ newLayer) hqdm

-- | printableTypeTree
-- Takes the output of findSupertypeTree or findSubtypeTree and renders it in a printable form - rather like ASCII art.
--
-- Inputs are a list of layers (tree) from findSupertypeTree and the HQDM all as data triples to query.
-- Output is a list of layers as a single printable string centred on an offset set by fmtString.
fmtString :: [Char] -> [Char]
fmtString x = replicate (screenCharOffset - div (length x) 2) ' ' ++ x

printableTypeTree :: [[Id]] -> [HqdmTriple] -> String -> String
printableTypeTree tree hqdmModel textTree
  | null (take 1 tree) = textTree
  | otherwise =
      printableTypeTree
        (tail tree)
        hqdmModel
        (textTree ++ fmtString (concatMap (\ x -> "[" ++ x ++ "] ") (findHqdmTypeNamesInList (head tree) hqdmModel)) ++ "\n" ++ HqdmLib.fmtString "^\n" ++ HqdmLib.fmtString "/|\\\n" ++ HqdmLib.fmtString "|\n")

-- | findSubtypeTree
-- From all the triples given by lookupSubtypes find the subtypes (and sub-classes) of a given node Id.
-- This takes only has_supertype statements as [HqdmTriple]
findSubtypeTree :: [[Id]] -> [HqdmHasSupertype] -> [[Id]]
findSubtypeTree ids hqdmStl = go ids hqdmStl
  where
    nextLayer = last ids
    possibleNewLayer = uniqueIds $ concat (lookupSubtypesOf nextLayer hqdmStl)
    newLayer = [deleteItemsFromList possibleNewLayer (concat ids)]

    go ids hqdmStl
      | null (head newLayer) = ids
      | otherwise = findSubtypeTree (ids ++ newLayer) hqdmStl

-------------------------------------------------
-- Take the result and compose a list of the relations inherited down the tree, via all paths

-- | findInheritedRels
-- Build the list of inherited relations for the given type. Do this from a calculated supertype tree (reversed).
--
-- Input: A given hqdm node id of the desired type.
-- Output: List of accumulated relations down the stack.
findInheritedRels :: [Id] -> [HqdmTriple] -> [[RelationPair]] -> [[RelationPair]]
findInheritedRels [] _ rels = rels
findInheritedRels tree hqdmModel rels =
  let nextType = headIfUUIDPresent tree
      newRels  = maybe [] (\uuid -> relationPairs $ lookupHqdmOne uuid hqdmModel) nextType
  in findInheritedRels (tail tree) hqdmModel (rels ++ [newRels])

-- Speed optimised version that avoids the ++ accumulator that has O(N^2) cost  This version has O(N) cost.
findInheritedRels' :: [Id] -> [HqdmTriple] -> [[RelationPair]] -> [[RelationPair]]
findInheritedRels' tree hqdmModel initialRels =
  -- 1. The Wrapper: calls the worker with an empty accumulator, 
  --    then appends the reversed results to the initialRels.
  initialRels ++ reverse (go tree [])
  where
    -- 2. The Worker: recurses efficiently using (:)
    go [] acc = acc
    go tRec@(_:ts) acc =
      let nextType = headIfUUIDPresent tRec
          newRels  = maybe [] (\uuid -> relationPairs $ lookupHqdmOne uuid hqdmModel) nextType
      in go ts (newRels : acc)

-- | collapseInheritedRels
-- Take a list of lists of RelationPairs and emit a single list of unique predicate names.
collapseInheritedRels :: [[RelationPair]] -> [UUID]
collapseInheritedRels [] = []
collapseInheritedRels rp = uniqueIds $ fmap p (concat rp)

-- | printableCollapsedList
-- Take a list of unique predicate names and replace any empty list with [" "]
printableCollapsedList :: [String] -> [String]
printableCollapsedList [] = [" "]
printableCollapsedList cl = fmap ("\n    " ++) cl

-- | printableRelationPair
-- Take a RelationPair, find the Type Name from the list of HqdmTriple triples and then emit the
-- predicate and type name as a joined String.
printableRelationPair :: RelationPair -> String
printableRelationPair rp = "    " ++ toString (p rp) ++ " " ++ toString (o rp) ++ "\n"

-- | printableRelationPairList
-- Take a list of RelationPairs and create a printable, concatenated String of their respective
-- predicate and type name Strings.
printableRelationPairList :: [RelationPair] -> String
printableRelationPairList rpl = concatMap printableRelationPair rpl ++ "\n"

-- | printableRelationPairs
-- Take a list of lists of RelationPairs and emit a printable, concatenated String of each of their
-- respective predicate and type name Strings (separating each list of RelationPairs by a rtn.)
printableRelationPairs :: [[RelationPair]] -> String
printableRelationPairs rpls = foldl (++) "\n" (fmap printableRelationPairList rpls)

-- | exportAsTriples
-- Take a list of tuples as [(NodeId, [RelationPair])] and emit a list of triples as [HqdmTriple]
-- exportAsTriples tpls
-- = fmap (\ x -> (HqdmTriple (fst tpl) (p x) (o x)))  (snd tpl): exportAsTriples tpls
-- OR
-- = map
--      (\ tpl
--         -> fmap (\ x -> HqdmTriple (fst tpl) (p x) (o x)) (snd tpl))
--      tpls
exportAsTriples :: [(Id, [RelationPair])] -> [[HqdmTriple]]
exportAsTriples = map
      (\ tpl
         -> fmap (\ x -> HqdmTriple (fst tpl) (p x) (o x)) (snd tpl))

csvTriplesFromHqdmTriples :: [HqdmTriple] -> [String]
csvTriplesFromHqdmTriples = fmap (\ x -> toString (subject x) ++ "," ++ toString (predicate x) ++ "," ++ toString (object x) ++ "\n")