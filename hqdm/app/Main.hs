{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import HqdmLib

-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (HasHeader( NoHeader ), decode)
-- import Data.List (sortUniq)
import qualified Data.Vector as V

main :: IO ()
main = do
    --file_contents <- readFile "hqdmAllAsDataNoParentheses.stmt"
     --putStrLn file_contents

    hqdmTriples <- fmap V.toList . decode @HqdmInput NoHeader <$> BL.readFile "hqdmAllAsData.csv"
    print hqdmTriples
    putStr "\n\nLoaded Data\n\n"

    let hqdmRawNodes = either (const ["Err"]) (\r -> getSubjects r) hqdmTriples
    
    -- Generate list of all hqdm:iris    
    let uniqueNodes = uniqueIds hqdmRawNodes

    putStr "\n\nList of ids:\n\n"
    print uniqueNodes

    putStr "\nNumber of ids is:\n\n"
    print (length uniqueNodes)

    let hqdmRawPredicates = either (const ["Err"]) (\r -> getPredicates r) hqdmTriples
    let uniquePredicates = uniqueIds hqdmRawPredicates

    putStr "\n\nList of predicates:\n\n"
    print uniquePredicates

    putStr "\nNumber of predicates is:\n\n"
    print (length uniquePredicates)
  
    -- Generate graph of iris aas vertexes and edges

    -- Compose the structural validations of hqdm
    
    putStr "\n\nDone\n"
    
