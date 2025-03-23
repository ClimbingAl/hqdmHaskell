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
    HqdmTriple(subject, predicate, object),
    RelationPair,
    Relation,
    HqdmHasSupertype,
    getSubjects,
    getPredicates,
    uniqueIds,
    uniqueTriples,
    stringListSort,
    headIfStringPresent,
    lookupHqdmOne,
    lookupHqdmTypeIdFromName,
    relationPairs,
    lookupHqdmType,
    lookupHqdmIdsFromTypePredicates,
    lookupHqdmTypeFromAll,
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
    printableRelationPairList,
    exportAsTriples,
    csvTriplesFromHqdmTriples,
    screenCharOffset,
    fmtString,
    deleteItemsFromList,
    nodeIdentityTest
  )
where

import Data.Csv (FromRecord)
import Data.List (elemIndices)
import GHC.Generics (Generic)
import HqdmIds (thing)

data HqdmTriple = HqdmTriple
  { subject :: !Id,
    predicate :: !Id,
    object :: !Id
  }
  deriving (Show, Eq, Generic)

instance FromRecord HqdmTriple

data RelationPair = RelationPair
  { p :: !Id,
    o :: !Id
  }
  deriving (Show, Eq, Generic)

newtype Relation = Relation
  { rel :: Id
  }
  deriving (Show, Eq, Generic)

-- type synonyms to handle the CSV contents
type Id = String

type Object = String

type Predicate = String

type HqdmHasSupertype = HqdmTriple

-- Names of relations relevant to importing triples
hqdmType::String
hqdmType = "type"

hqdmTypeId::String
hqdmTypeId = "7e249a64-9f13-47d3-a232-562a3d080198"

hqdmHasSupertype::String
hqdmHasSupertype = "has_supertype"

hqdmHasSupertypeId::String
hqdmHasSupertypeId = "1f983e8a-7db1-4374-8fb1-7e8a432a967e"

hqdmHasSuperclass::String
hqdmHasSuperclass = "has_superclass"

hqdmHasSuperclassId::String
hqdmHasSuperclassId = "7d11b956-0014-43be-9a3e-f89e2b31ec4f"

screenCharOffset :: Int
screenCharOffset = 100

-- | nodeIdentityTest
-- Check that an element (subject or object) is a Node Id.  Done without regex to avoid dependencies.
-- Expects to be True for a String of the form "130e95f1-ebc4-46f1-90ba-3f9fa21cb77b"
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
headIfStringPresent :: [String] -> String
headIfStringPresent x
  | not (null x)   = head x
  | otherwise      = ""

-------------------------------------------------
-- Based on an online source (not covered by Copyright):
-- function declaration for function insert
insert :: [String] -> String -> [String]
-- function definition for function insert
-- base case
insert [] y = [y]
insert (x : xs) y =
  if y < x
    then [y] ++ [x] ++ xs
    else x : insert xs y

-- function declaration for function insert
stringListSort :: [String] -> [String]
stringListSort [] = []
stringListSort (x : xs) = insert (stringListSort xs) x

deleteItemsFromList :: [String] -> [String] -> [String]
deleteItemsFromList fromList itemsToRemove = [x | x <- fromList, x `notElem` itemsToRemove]

-------------------------------------------------------------------------------------
-- End of Utils
-------------------------------------------------------------------------------------

-------------------------------------------------
-- GRAPH FUNCTIONS USING THE LIST OF TRIPLES OF HqdmAllAsData

-- | lookupHqdmOne
-- Find all the triples that have the given node Id (subject).
lookupHqdmOne :: Id -> [HqdmTriple] -> [HqdmTriple]
lookupHqdmOne x list = [values | values <- list, x == subject values]

-- | relationPairs
-- Take a list of triples (typically with the same node Id from lookupHqdmOne) and return a list of the relation pairs.
relationPairs :: [HqdmTriple] -> [RelationPair]
relationPairs = fmap (\x -> RelationPair (predicate x) (object x))

-- | lookupHqdmTypeFromAll
-- From the complete set of HQDM triples with a given node Id (subject), from lookupHqdmOne, find the type name of the given Node Id.
lookupHqdmTypeFromAll :: [HqdmTriple] -> String -> [String]
lookupHqdmTypeFromAll hqdmAll nodeId = [object values | values <- hqdmAll, ((hqdmType == predicate values) || ( hqdmTypeId == predicate values)) && (nodeId == subject values)]

-- | lookupHqdmTypeIdFromName
-- From a set of HQDM AllAsData triples, find the ObjectId from a given Type Name
lookupHqdmTypeIdFromName :: [HqdmTriple] -> String -> String
lookupHqdmTypeIdFromName hqdmAll typeName = headIfStringPresent [subject values | values <- hqdmAll, ((hqdmType == predicate values) || ( hqdmTypeId == predicate values)) && (typeName == object values)]

-- | lookupHqdmType
-- From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate type.
lookupHqdmType :: [HqdmTriple] -> [String]
lookupHqdmType obj = [object values | values <- obj, (hqdmType == predicate values) || ( hqdmTypeId == predicate values)]

-- | lookupHqdmIdsFromTypePredicates
-- From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate type.
lookupHqdmIdsFromTypePredicates :: [HqdmTriple] -> String -> [Id]
lookupHqdmIdsFromTypePredicates objs typeName
  | nodeIdentityTest typeName = [typeName]
  | otherwise = [subject values | values <- objs, ((hqdmType == predicate values) || ( hqdmTypeId == predicate values)) && (typeName == object values)]

-- | findHqdmTypesInList
-- Find the type names of each given node Id (subject).
findHqdmTypeNamesInList :: [Id] -> [HqdmTriple] -> [String]
findHqdmTypeNamesInList ids hqdmModel = fmap (\ x -> headIfStringPresent (lookupHqdmType $ lookupHqdmOne x hqdmModel)) ids

-- | lookupSubtypes
-- From all the triples that have the has_supertype or has_superclass predicate.
lookupSubtypes :: [HqdmTriple] -> [HqdmTriple]
lookupSubtypes list =
  [ values
    | values <- list,
      (hqdmHasSupertype == predicate values) || (hqdmHasSuperclass == predicate values)
  ] ++ [
    values
    | values <- list,
      (hqdmHasSupertypeId == predicate values) || (hqdmHasSuperclassId == predicate values)
  ]

-- | lookupSubtypeOf
-- From all the triples given by lookupSubtypes find the subtypes of a given node Id.
-- This takes only has_supertype statements as [HqdmTriple]
lookupSubtypeOf :: Id -> [HqdmTriple] -> [Id]
lookupSubtypeOf x list = [subject values | values <- list, x == object values]

-- | lookupSubtypesOf
-- Same as lookupSubtypeOf but takes a list of Ids and finds a list of subtypes for each.
lookupSubtypesOf :: [Id] -> [HqdmTriple] -> [[Id]]
lookupSubtypesOf [] _ = []
lookupSubtypesOf _ [] = []
lookupSubtypesOf (id : ids) list = lookupSubtypeOf id list : lookupSubtypesOf ids list

-- | lookupSupertypOf
-- From all the triples given by lookupSubtypes find the supertypes of a given node Id.
-- This takes only has_supertype statements as [HqdmTriple]
lookupSupertypeOf :: Id -> [HqdmTriple] -> [Id]
lookupSupertypeOf x list = [object values | values <- list, x == subject values && ((hqdmHasSupertype == predicate values) || (hqdmHasSuperclass == predicate values) || (hqdmHasSupertypeId == predicate values) || (hqdmHasSuperclassId == predicate values))]

-- | lookupSupertypesOf
-- Same as lookupSupertypeOf but takes a list of Ids and finds a list of supertypes for each.
lookupSupertypesOf :: [Id] -> [HqdmTriple] -> [[Id]]
lookupSupertypesOf [] _ = []
lookupSupertypesOf _ [] = []
lookupSupertypesOf (id : ids) list = lookupSupertypeOf id list : lookupSupertypesOf ids list

-------------------------------------------------
-- | findHqdmTypesInList
-- Find the type names of the Node Ids supplied as a list of Strings.  Takes HQDM AllAsData as input.
findHqdmTypesInList :: [Id] -> [HqdmTriple] -> [String]
findHqdmTypesInList xs hqdmIn = fmap (\ x -> headIfStringPresent (lookupHqdmType $ lookupHqdmOne x hqdmIn)) xs

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
findInheritedRels tree hqdmModel rels = go tree hqdmModel rels
  where
    nextType = headIfStringPresent tree
    newRels = relationPairs $ lookupHqdmOne nextType hqdmModel

    go tree hqdmModel rels
      | null (take 1 tree) = rels
      | otherwise = findInheritedRels (tail tree) hqdmModel (rels ++ [newRels])

-- | collapseInheritedRels
-- Take a list of lists of RelationPairs and emit a single list of unique predicate names.
collapseInheritedRels :: [[RelationPair]] -> [String]
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
printableRelationPair :: [HqdmTriple] -> RelationPair -> String
printableRelationPair hqdmAll rp
  | nodeIdentityTest (o rp) = "    " ++ p rp ++ " [" ++ headIfStringPresent (lookupHqdmTypeFromAll hqdmAll (o rp)) ++ "]\n"
  | otherwise = "    " ++ p rp ++ " " ++ o rp ++ "\n"

-- | printableRelationPairList
-- Take a list of RelationPairs and create a printable, concatenated String of their respective
-- predicate and type name Strings.
printableRelationPairList :: [HqdmTriple] -> [RelationPair] -> String
printableRelationPairList hqdmAll rpl = concatMap (printableRelationPair hqdmAll) rpl ++ "\n"

-- | printableRelationPairs
-- Take a list of lists of RelationPairs and emit a printable, concatenated String of each of their
-- respective predicate and type name Strings (separating each list of RelationPairs by a rtn.)
printableRelationPairs :: [HqdmTriple] -> [[RelationPair]] -> String
printableRelationPairs hqdmAll rpls = foldl (++) "\n" (fmap (printableRelationPairList hqdmAll) rpls)

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
csvTriplesFromHqdmTriples = fmap (\ x -> subject x ++ "," ++ predicate x ++ "," ++ object x ++ "\n")