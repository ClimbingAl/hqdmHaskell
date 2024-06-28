{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}


module Main (main) where

import Lib
import System.IO

import GHC.Generics (Generic)
import System.Exit (exitFailure)
-- from bytestring
import Data.ByteString (ByteString, hGetSome, empty)
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv (FromRecord, ToRecord, Record, HasHeader( NoHeader ), decode)
-- import Data.List (sortUniq)
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Text.Printf

-- import qualified Text as T
import qualified Data.Text as T

type Predicate = T.Text

-- main :: IO ()
-- main = someFunc

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

type CsvData = (V.Vector Record)

getSubjects :: [HqdmInput] -> [String]
getSubjects xs = map (subject) xs

uniqueIds :: [String] -> [String]
uniqueIds xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]


main                    :: IO ()
main = do
    --file_contents <- readFile "hqdmAllAsDataNoParentheses.stmt"
     --putStrLn file_contents

    hqdmTriples <- fmap V.toList . decode @HqdmInput NoHeader <$> BL.readFile "hqdmAllAsData.csv"
    print hqdmTriples
    putStr "\n\nLoaded Data\n\n"

    let hqdmRawNodes = either (const ["Err"]) (\r -> getSubjects r) hqdmTriples
    
    -- Generate list of all hqdm:iris    
    let uniqueNodes = uniqueIds hqdmRawNodes

    putStr "\nznList of ids:\n\n"
    print uniqueNodes

    putStr "\nNumber of ids is:\n\n"
    print (length uniqueNodes)

    putStr "\n\nDone\n"

  
    -- Generate graph of iris aas vertexes and edges

    -- Compose the structural validation of hqdm
    

    
