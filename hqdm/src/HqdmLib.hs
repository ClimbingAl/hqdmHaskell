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
    HqdmInput,
    RelationPair,
    Relation,
    HqdmHasSupertype,
    subject,
    getSubjects,
    getPredicates,
    uniqueIds,
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
  )
where

import Data.Csv (FromRecord)
import Data.List (elemIndices)
import GHC.Generics (Generic)

data HqdmInput = HqdmInput
  { subject :: !String,
    predicate :: !String,
    object :: !String
  }
  deriving (Show, Eq, Generic)

instance FromRecord HqdmInput

data RelationPair = RelationPair
  { p :: !String,
    o :: !String
  }
  deriving (Show, Eq, Generic)

newtype Relation = Relation
  { rel :: String
  }
  deriving (Show, Eq, Generic)

-- type synonyms to handle the CSV contents
type Id = String

type Object = String

type Predicate = String

type HqdmHasSupertype = HqdmInput

screenCharOffset :: Int
screenCharOffset = 100

-- | nodeIdentityTest
-- Check that an element (subject or object) is a Node Id.  Done without regex to avoid dependencies.
-- Expects to be True for a String of the form "hqdm:130e95f1-ebc4-46f1-90ba-3f9fa21cb77b"
nodeIdentityTest :: String -> Bool
nodeIdentityTest x = length x == 41 && ('-' `elemIndices` x) == [13, 18, 23, 28]

getSubjects :: [HqdmInput] -> [Id]
getSubjects = map subject

getPredicates :: [HqdmInput] -> [Predicate]
getPredicates = map predicate

getObjects :: [HqdmInput] -> [Object]
getObjects xs = map object xs -- Can't Eta reduce due to "object" name collision

uniqueIds :: [Id] -> [Id]
uniqueIds xs = [x | (x, y) <- zip xs [0 ..], x `notElem` take y xs]

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

-------------------------------------------------
-- GRAPH FUNCTIONS USING THE LIST OF TRIPLES OF HqdmAllAsData

-- | lookupHqdmOne
-- Find all the triples that have the given node Id (subject).
lookupHqdmOne :: Id -> [HqdmInput] -> [HqdmInput]
lookupHqdmOne x list = [values | values <- list, x == subject values]

-- | relationPairs
-- Take a list of triples (typically with the samed node Id from lookupHqdmOne) and return a list of the relation pairs.
relationPairs :: [HqdmInput] -> [RelationPair]
relationPairs = fmap (\x -> RelationPair (predicate x) (object x))

-- | lookupHqdmTypeFromAll
-- From the complete set of HQDM triples with a given node Id (subject), from lookupHqdmOne, find the type name of the given Node Id.
lookupHqdmTypeFromAll :: [HqdmInput] -> String -> [String]
lookupHqdmTypeFromAll hqdmAll nodeId = [object values | values <- hqdmAll, ("hqdm:type" == predicate values) && (nodeId == subject values)]

-- | lookupHqdmType
-- From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate hqdm:type.
lookupHqdmType :: [HqdmInput] -> [String]
lookupHqdmType obj = [object values | values <- obj, "hqdm:type" == predicate values]

-- | findHqdmTypesInList
-- Find the type names of each given node Id (subject).
findHqdmTypeNamesInList :: [Id] -> [HqdmInput] -> [String]
findHqdmTypeNamesInList ids hqdmModel = fmap (\ x -> head (lookupHqdmType $ lookupHqdmOne x hqdmModel)) ids

-- | lookupSubtypes
-- From all the triples that have the hqdm:has_supertype or hqdm:has_superclass predicate.
lookupSubtypes :: [HqdmInput] -> [HqdmInput]
lookupSubtypes list =
  [ values
    | values <- list,
      ("hqdm:has_supertype" == predicate values) || ("hqdm:has_superclass" == predicate values)
  ]

-- | lookupSubtypeOf
-- From all the triples given by lookupSubtypes find the subtypes of a given node Id.
-- This takes only hqdm:has_supertype statements as [HqdmInput]
lookupSubtypeOf :: Id -> [HqdmInput] -> [Id]
lookupSubtypeOf x list = [subject values | values <- list, x == object values]

-- | lookupSubtypesOf
-- Same as lookupSubtypeOf but takes a list of Ids and finds a list of subtypes for each.
lookupSubtypesOf :: [Id] -> [HqdmInput] -> [[Id]]
lookupSubtypesOf [] _ = []
lookupSubtypesOf _ [] = []
lookupSubtypesOf (id : ids) list = lookupSubtypeOf id list : lookupSubtypesOf ids list

-- | lookupSupertypOf
-- From all the triples given by lookupSubtypes find the supertypes of a given node Id.
-- This takes only hqdm:has_supertype statements as [HqdmInput]
lookupSupertypeOf :: Id -> [HqdmInput] -> [String]
lookupSupertypeOf x list = [object values | values <- list, x == subject values]

-- | lookupSupertypesOf
-- Same as lookupSupertypeOf but takes a list of Ids and finds a list of supertypes for each.
lookupSupertypesOf :: [Id] -> [HqdmInput] -> [[String]]
lookupSupertypesOf [] _ = []
lookupSupertypesOf _ [] = []
lookupSupertypesOf (id : ids) list = lookupSupertypeOf id list : lookupSupertypesOf ids list

-------------------------------------------------
-- | findHqdmTypesInList
-- Find the type names of the Node Ids supplied as a list of Strings.  Takes HQDM AllAsData as input.
findHqdmTypesInList :: [String] -> [HqdmInput] -> [String]
findHqdmTypesInList xs hqdmIn = fmap (\ x -> head (lookupHqdmType $ lookupHqdmOne x hqdmIn)) xs

-- | findSupertypeTree
-- From all the triples given by lookupSupertypes find all the supertypes of a given node Id
-- (supplied as a [[id]]). This takes only hqdm:has_supertype statements as [HqdmInput].
-- The ouput is a list of layers from the supplied subtype to the termination empty layer
-- above hqdm:thing.
findSupertypeTree :: [[Id]] -> [HqdmHasSupertype] -> [[Id]]
findSupertypeTree ids hqdm = go ids hqdm
  where
    nextLayer = last ids
    possibleNewLayer = concat (take 1 (lookupSupertypesOf nextLayer hqdm))
    newLayer = [deleteItemsFromList possibleNewLayer (take 1 nextLayer)]
    -- newLayer is formed from a defence against circularity.  Remove elements of newLayer that are in nextLayer.

    go ids hqdm
      | null newLayer = init ids
      | newLayer == [[]] = ids
      | sum [length $ filter (== "hqdm:e5ec5d9e-afea-44f7-93c9-699cd5072d90") yl | yl <- newLayer] > 0 = ids ++ newLayer
      | otherwise = findSupertypeTree (ids ++ newLayer) hqdm

-- | printableTypeTree
-- Takes the output of findSupertypeTree or findSubtypeTree and renders it in a printable form - rather like ASCII art.
--
-- Inputs are a list of layers (tree) from findSupertypeTree and the HQDM all as data triples to query.
-- Output is a list of layers as a single printable string centred on an offset set by fmtString.
fmtString x = replicate (screenCharOffset - div (length x) 2) ' ' ++ x

printableTypeTree :: [[String]] -> [HqdmInput] -> String -> String
printableTypeTree tree hqdmModel textTree
  | null (take 1 tree) = textTree
  | otherwise =
      printableTypeTree
        (tail tree)
        hqdmModel
        (textTree ++ fmtString (unwords (findHqdmTypeNamesInList (head tree) hqdmModel)) ++ "\n\n")

-- | findSubtypeTree
-- From all the triples given by lookupSubtypes find the subtypes (and sub-classes) of a given node Id.
-- This takes only hqdm:has_supertype statements as [HqdmInput]
findSubtypeTree :: [[Id]] -> [HqdmInput] -> [Id] -> [[String]]
findSubtypeTree ids hqdm previousIds = go ids hqdm previousIds
  where
    nextLayer = last ids
    newLayer = [uniqueIds $ concat (lookupSubtypesOf nextLayer hqdm)]
    --- Add defence against circularity.  Remove elements of newLayer that are in nextLayer.

    go ids hqdm previousIds
      | head newLayer == previousIds = ids
      | otherwise = findSubtypeTree (ids ++ newLayer) hqdm (head newLayer)

-------------------------------------------------
-- Take the result and compose a list of the relations inherited down the tree, via all paths

-- | findInheritedRels
-- Build the list of inherited relations for the given type. Do this from a calculated supertype tree (reversed).
--
-- Input: A given hqdm node id of the desired type.
-- Output: List of accumulated relations down the stack.
findInheritedRels :: [Id] -> [HqdmInput] -> [[RelationPair]] -> [[RelationPair]]
findInheritedRels tree hqdmModel rels = go tree hqdmModel rels
  where
    nextType = head tree
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
-- Take a RelationPair, find the Type Name from the list of HqdmInput triples and then emit the
-- predicate and type name as a joined String.
printableRelationPair :: [HqdmInput] -> RelationPair -> String
printableRelationPair hqdmAll rp
  | nodeIdentityTest (o rp) = "    " ++ p rp ++ " " ++ head (lookupHqdmTypeFromAll hqdmAll (o rp)) ++ "\n"
  | otherwise = "    " ++ p rp ++ " " ++ o rp ++ "\n"

-- | printableRelationPairList
-- Take a list of RelationPairs and create a printable, concatenated String of their respective
-- predicate and type name Strings.
printableRelationPairList :: [HqdmInput] -> [RelationPair] -> String
printableRelationPairList hqdmAll rpl = concatMap (printableRelationPair hqdmAll) rpl ++ "\n"

-- | printableRelationPairs
-- Take a list of lists of RelationPairs and emit a printable, concatenated String of each of their
-- respective predicate and type name Strings (separating each list of RelationPairs by a rtn.)
printableRelationPairs :: [HqdmInput] -> [[RelationPair]] -> String
printableRelationPairs hqdmAll rpls = foldl (++) "\n" (fmap (printableRelationPairList hqdmAll) rpls)
