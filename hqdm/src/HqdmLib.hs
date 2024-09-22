{-# LANGUAGE DeriveGeneric #-}

module HqdmLib 
( HqdmInput,
subject,
getSubjects,
getPredicates,
uniqueIds,
stringListSort,
lookupHqdmOne,
lookupHqdmType,
lookupSubtypes,
lookupSubtypeOf,
lookupSupertypeOf,
lookupSupertypesOf,
findSupertypeTree
) where

import GHC.Generics (Generic)
import Data.Csv (FromRecord)

data HqdmInput = HqdmInput
    {
        subject :: !String,
        predicate :: !String,
        object :: !String
    }
    deriving (Show, Eq, Generic)

instance FromRecord HqdmInput

-- let emptyHqdmInput = HqdmInput "" "" ""

-- type synonyms to handle the CSV contents
type ErrorMsg = String
type Id = String

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

{- | lookupHqdmType
From the triples with a given node Id (subject), from lookupHqdmOne, find the object with the predicate rdf:type.
-}
lookupHqdmType :: [HqdmInput] -> [String]
lookupHqdmType obj = [object values | (values)<-obj, "rdf:type"==(predicate values)]

{- | lookupSubtypes
From all the triples that have the hqdm:has_supertype or hqdm:has_superclass predicate.
-}
lookupSubtypes :: [HqdmInput] -> [HqdmInput]
lookupSubtypes list = [values | (values)<-list, ("hqdm:has_supertype"==(predicate values))||("hqdm:has_superclass"==(predicate values))]

{- | lookupSubtypeOf
From all the triples given by lookupSubtypes find the subtypes of a given node Id.
This takes only hqdm:has_supertype statements as [HqdmInput]
-}
lookupSubtypeOf :: Id -> [HqdmInput] -> [String]
lookupSubtypeOf x list = [subject values | (values)<-list, x==(object values)]

{- | lookupSupertypOf
From all the triples given by lookupSubtypes find the supertypes of a given node Id.
This takes only hqdm:has_supertype statements as [HqdmInput]
-}
lookupSupertypeOf :: Id -> [HqdmInput] -> [String]
lookupSupertypeOf x list = [object values | (values)<-list, x==(subject values)]

lookupSupertypesOf :: [Id] -> [HqdmInput] -> [[String]]
lookupSupertypesOf [] _ = []
lookupSupertypesOf _ [] = []
lookupSupertypesOf (id:ids) list = lookupSupertypeOf id list : lookupSupertypesOf ids list

-------------------------------------------------
-- Find subtypes and subclasses
{- | findSubtypeTree
From all the triples given by lookupSubtypes find the supertypes of a given node Id.
This takes only hqdm:has_supertype statements as [HqdmInput]
-}


-- Find top level type
{- | findSupertypeTree
From all the triples given by lookupSubtypes find all the supertypes of a given node Id
(supplied as a [[id]]). This takes only hqdm:has_supertype statements as [HqdmInput].
The ouput is a list of layers from the supplied subtype to the termination empty layer 
above hqdm:thing.
-}
findSupertypeTree :: [[Id]] -> [HqdmInput] -> [[String]]
findSupertypeTree ids hqdm = go ids hqdm
 where
    nextLayer = last ids
    newLayer = lookupSupertypesOf nextLayer hqdm
    
    go ids hqdm
        | newLayer==[]  = ids
        | otherwise     = findSupertypeTree (ids ++ newLayer) hqdm

{- | printableSupertypeTree
Takes the output of findSupertypeTree and renders it in a printable form - rather like ASCII art.
-}
--printableSupertypeTree