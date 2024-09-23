{-# LANGUAGE DeriveGeneric #-}

module HqdmLib
( HqdmInput,
RelationPair,
Relation,
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
findSupertypeTree,
printableTypeTree,
findSubtypeTree,
findInheritedRels,
collapseInheritedRels
) where

import GHC.Generics (Generic)
import Data.Csv (FromRecord)
import Data.Int (Int)
import GHC.IO.FD (release)

data HqdmInput = HqdmInput
    {
        subject :: !String,
        predicate :: !String,
        object :: !String
    }
    deriving (Show, Eq, Generic)

instance FromRecord HqdmInput

data RelationPair = RelationPair
    {
        p :: !String,
        o :: !String
    }
    deriving (Show, Eq, Generic)

newtype Relation = Relation
    {
        rel :: String
    }
    deriving (Show, Eq, Generic)

-- let emptyHqdmInput = HqdmInput "" "" ""

-- type synonyms to handle the CSV contents
type ErrorMsg = String
type Id = String

screenCharOffset::Int
screenCharOffset = 200

getSubjects :: [HqdmInput] -> [String]
getSubjects xs = map (subject) xs

getPredicates :: [HqdmInput] -> [String]
getPredicates xs = map (predicate) xs

getObjects :: [HqdmInput] -> [String]
getObjects xs = map (object) xs

--getEdges :: [HqdmInput] -> [LEdge]
--getEdges xs = map (LEdge (subject object predicate))

uniqueIds :: [String] -> [String]
uniqueIds xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]

-- | File IO stuff
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp

-------------------------------------------------
-- Based on an online source:
-- function declaration for function insert
insert :: [String]->String->[String]

-- function definition for function insert
-- base case
insert [] y = [y]
insert (x:xs) y = if y < x
                        then [y]++[x]++xs
                        else [x]++(insert xs y)

-- function declaration for function insert
stringListSort :: [String]->[String]

-- function definition for function insert
-- base case
stringListSort [] = []
stringListSort (x:xs) = insert (stringListSort xs) x

----------------------------------------------
-- GRAPH FUNCTIONS USING THE LIST OF TRIPLES OF HqdmAllAsData

{- | lookupHqdmOne
Find all the triples that have the given node Id (subject).
-}
lookupHqdmOne :: Id -> [HqdmInput] -> [HqdmInput]
lookupHqdmOne x list = [values | (values)<-list, x==(subject values)]

{- | relationPairs
Take a list of triples (typically with the samed node Id from lookupHqdmOne) and return a list of the relation pairs.
-}
relationPairs :: [HqdmInput] -> [RelationPair]
relationPairs [] = []
relationPairs (x:xs) =  RelationPair (predicate x) (object x) : relationPairs xs

{- | lookupHqdmType
From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate rdf:type.
-}
lookupHqdmType :: [HqdmInput] -> [String]
lookupHqdmType obj = [object values | (values)<-obj, "rdf:type"==(predicate values)]

{- | findHqdmTypesInList
Find the type names of each given node Id (subject).
-}
findHqdmTypeNamesInList :: [String] -> [HqdmInput] -> [String]
findHqdmTypeNamesInList ids hqdmModel = fmap (\x -> head (lookupHqdmType $ lookupHqdmOne x hqdmModel)) ids

{- | lookupSubtypes
From all the triples that have the hqdm:has_supertype or hqdm:has_superclass predicate.
-}
lookupSubtypes :: [HqdmInput] -> [HqdmInput]
lookupSubtypes list = [values 
    | (values)<-list, ("hqdm:has_supertype"==(predicate values))||("hqdm:has_superclass"==(predicate values))]

{- | lookupSubtypeOf
From all the triples given by lookupSubtypes find the subtypes of a given node Id.
This takes only hqdm:has_supertype statements as [HqdmInput]
-}
lookupSubtypeOf :: Id -> [HqdmInput] -> [String]
lookupSubtypeOf x list = [subject values | (values)<-list, x==(object values)]

{- | lookupSubtypesOf
Same as lookupSubtypeOf but takes a list of Ids and finds a list of subtypes for each.
-}
lookupSubtypesOf :: [Id] -> [HqdmInput] -> [[String]]
lookupSubtypesOf [] _ = []
lookupSubtypesOf _ [] = []
lookupSubtypesOf (id:ids) list = lookupSubtypeOf id list : lookupSubtypesOf ids list

{- | lookupSupertypOf
From all the triples given by lookupSubtypes find the supertypes of a given node Id.
This takes only hqdm:has_supertype statements as [HqdmInput]
-}
lookupSupertypeOf :: Id -> [HqdmInput] -> [String]
lookupSupertypeOf x list = [object values | (values)<-list, x==(subject values)]

{- | lookupSupertypesOf
Same as lookupSupertypeOf but takes a list of Ids and finds a list of supertypes for each.
-}
lookupSupertypesOf :: [Id] -> [HqdmInput] -> [[String]]
lookupSupertypesOf [] _ = []
lookupSupertypesOf _ [] = []
lookupSupertypesOf (id:ids) list = lookupSupertypeOf id list : lookupSupertypesOf ids list

-------------------------------------------------
{- | findSupertypeTree
From all the triples given by lookupSupertypes find all the supertypes of a given node Id
(supplied as a [[id]]). This takes only hqdm:has_supertype statements as [HqdmInput].
The ouput is a list of layers from the supplied subtype to the termination empty layer 
above hqdm:thing.
-}
findSupertypeTree :: [[Id]] -> [HqdmInput] -> [[String]]
findSupertypeTree ids hqdm = go ids hqdm
 where
    nextLayer = last ids
    newLayer = [ concat (take 1 (lookupSupertypesOf nextLayer hqdm)) ]

    go ids hqdm
        | newLayer==[]  = init ids
        | sum [length $ filter (=="hqdm:e5ec5d9e-afea-44f7-93c9-699cd5072d90") yl | yl <- newLayer] > 0  = ids ++ newLayer
        | otherwise     = findSupertypeTree (ids ++ newLayer) hqdm

{- | printableTypeTree
Takes the output of findSupertypeTree or findSubtypeTree and renders it in a printable form - rather like ASCII art.

Inputs are a list of layers (tree) from findSupertypeTree and the HQDM all as data triples to query. 
Output is a list of layers as a single printable string centred on an offset set by fmtString.
-}
fmtString x = replicate (screenCharOffset - div (length x) 2) ' ' ++ x

printableTypeTree :: [[String]] -> [HqdmInput] -> String -> String
printableTypeTree tree hqdmModel textTree
    | take 1 tree == [] = textTree
    | otherwise         =
        printableTypeTree
            (tail tree)
            hqdmModel
            ( textTree ++  fmtString (unwords (findHqdmTypeNamesInList (head tree) hqdmModel)) ++ "\n\n" )

{- | findSubtypeTree
From all the triples given by lookupSubtypes find the subtypes (and sub-classes) of a given node Id.
This takes only hqdm:has_supertype statements as [HqdmInput]
-}
findSubtypeTree :: [[Id]] -> [HqdmInput] -> [Id] ->  [[String]]
findSubtypeTree ids hqdm previousIds = go ids hqdm previousIds
    where
        nextLayer = last ids
        newLayer = [ uniqueIds $ concat (lookupSubtypesOf nextLayer hqdm) ]

        go ids hqdm previousIds
            | (head newLayer)==previousIds  = ids
            | otherwise                     = findSubtypeTree (ids ++ newLayer) hqdm (head newLayer)

-------------------------------------------------
-- Take the result and compose a list of the relations inherited down the tree, via all paths
{- | findInheritedRels
Build the list of inherited relations for the given type. Do this from a calculated supertype tree (reversed).

Input: A given hqdm node id of the desired type.Applicative
Output: List of accumulated relations down the stack. 
-}
findInheritedRels :: [Id] -> [HqdmInput] -> [[RelationPair]] -> [[RelationPair]]
findInheritedRels tree hqdmModel rels = go tree hqdmModel rels
    where
        nextType = head tree
        newRels = relationPairs $ lookupHqdmOne nextType hqdmModel

        go tree hqdmModel rels
            | take 1 tree == []   = rels
            | otherwise           = findInheritedRels (tail tree) hqdmModel (rels ++ [newRels])


{- | collapseInheritedRels
-}
collapseInheritedRels :: [[RelationPair]] -> [String]
collapseInheritedRels [] = []
collapseInheritedRels rp = uniqueIds $ fmap p (concat rp)
