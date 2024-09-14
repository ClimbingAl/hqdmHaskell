{-# LANGUAGE DeriveGeneric #-}

module HqdmLib 
( HqdmInput,
subject,
getSubjects,
getPredicates,
uniqueIds,
stringListSort
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
-- GRAPH FEATURES BASED ON FGL

-- | Unlabeled node
type  Node   = String
-- | Labeled node
type LNode a = (Node, a)

lookupOne :: Int -> [(Int,a)] -> [a]
lookupOne x list = [values | (key,values)<-list, x==key]

lookupAll :: [String] -> [(String,a)] -> [a]
lookupAll xs list = [values | (key,values) <- list, key `elem` xs]

--getLNodes :: [HqdmInput] -> [String]
--getLNodes hi = map (lookupAll "rdf:type")



-- | Labeled edge
type LEdge b = (Node,Node,b)

-- | Labeled links to or from a 'Node'.
type Adj b        = [(b,Node)]
-- | Links to the 'Node', the 'Node' itself, a label, links from the 'Node'.
--
--   In other words, this captures all information regarding the
--   specified 'Node' within a graph.
type Context a b  = (Adj b,Node,a,Adj b) -- Context a b "=" Context' a b "+" Node
type MContext a b = Maybe (Context a b)

-- | Minimum implementation: 'empty', 'isEmpty', 'match', 'mkGraph', 'labNodes'
class Graph gr where
  {-# MINIMAL empty, isEmpty, mkGraph, labNodes #-}

  -- | An empty 'Graph'.
  empty     :: gr a b

  -- | True if the given 'Graph' is empty.
  isEmpty   :: gr a b -> Bool

  -- | Decompose a 'Graph' into the 'MContext' found for the given node and the
  -- remaining 'Graph'.
  --match     :: Node -> gr a b -> Decomp gr a b

  -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  --
  --   For graphs that are also instances of 'DynGraph', @mkGraph ns
  --   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
  --   'empty'@.
  mkGraph   :: [LNode a] -> [LEdge b] -> gr a b

  -- | A list of all 'LNode's in the 'Graph'.
  labNodes  :: gr a b -> [LNode a]

