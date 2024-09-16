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
lookupSupertypeOf
) where

import GHC.Generics (Generic)
import Data.Csv (FromRecord)
import Data.ByteString (isPrefixOf)

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
type Subject = String

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

-- | Unlabeled node
type  Node   = String
-- | Labeled node
type LNode a = (Node, a)

lookupOne :: String -> [(String,a)] -> [a]
lookupOne x list = [values | (key,values)<-list, x==key]

lookupHqdmOne :: String -> [HqdmInput] -> [HqdmInput]
lookupHqdmOne x list = [values | (values)<-list, x==(subject values)]

lookupHqdmType :: [HqdmInput] -> [String]
lookupHqdmType obj = [object values | (values)<-obj, "rdf:type"==(predicate values)]

lookupSubtypes :: [HqdmInput] -> [HqdmInput]
lookupSubtypes list = [values | (values)<-list, ("hqdm:has_supertype"==(predicate values))||("hqdm:has_superclass"==(predicate values))]

-- This takes only hqdm:has_supertype statements as [HqdmInput]
lookupSubtypeOf :: String -> [HqdmInput] -> [String]
lookupSubtypeOf x list = [subject values | (values)<-list, x==(object values)]

lookupSupertypeOf :: String -> [HqdmInput] -> [String]
lookupSupertypeOf x list = [object values | (values)<-list, x==(subject values)]

lookupAll :: [String] -> [(String,a)] -> [a]
lookupAll xs list = [values | (key,values) <- list, key `elem` xs]

-- Find subtypes and subclasses

-- Find top level type


--getLNodes :: [HqdmInput] -> [String]
--getLNodes hi = map (lookupAll "rdf:type")

