module Lib 
( someFunc,
Vertex,
graphFromEdges
) where


import Data.Array as Array
import System.IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)

graphFromEdges :: [(Vertex, [(Vertex, a)])] -> Graph a
graphFromEdges edges = Array.array bounds edges
  where
    bounds = (0, length edges - 1)

-- | All vertices of a graph.
vertices :: Graph a -> [Vertex]
vertices = Array.indices

-- | All edges of a graph.
getEdges :: Graph a -> [Edge a]
getEdges g = [ (v, w, a) | v <- vertices g, (w, a) <- g Array.! v ]

-- | File IO stuff
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp

