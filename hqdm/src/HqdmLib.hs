{-# LANGUAGE DeriveGeneric #-}

module HqdmLib 
( HqdmInput,
getSubjects,
getPredicates,
uniqueIds,
Vertex
) where


import Data.Array as Array ( Array, (!), array, indices )
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

-- type synonyms to handle the CSV contents
type ErrorMsg = String
type Subject = String

getSubjects :: [HqdmInput] -> [String]
getSubjects xs = map (subject) xs

getPredicates :: [HqdmInput] -> [String]
getPredicates xs = map (predicate) xs

uniqueIds :: [String] -> [String]
uniqueIds xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]

type Vertex  = Subject
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)

--  graphFromEdges :: [(Vertex, [(Vertex, a)])] -> Graph a
--  graphFromEdges edges = Array.array bounds edges
--    where
--    bounds = (0, length edges - 1)

-- | All vertices of a graph.
--  vertices :: Graph a -> [Vertex]
--  vertices = Array.indices

-- | All edges of a graph.
--  getEdges :: Graph a -> [Edge a]
--  getEdges g = [ (v, w, a) | v <- vertices g, (w, a) <- g Array.! v ]

-- | File IO stuff
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp

