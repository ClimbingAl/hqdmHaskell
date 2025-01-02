--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmCardinalityChecker Main
-- Description :  Takes a mapped dataset (mapped to hqdmHaskell by HqdmMapToPure) and performs a cardinality check on all relations/
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that implements the command line tool functionality.
--  HqdmCardinalityChecker -spoe hqdmRelations.scv hqdmEntityTypes.csv inputFile.csv 
-- Switches: 'h' Contains a header

module Main (main) where

import HqdmRelations (
    HqdmBinaryRelation,
    csvRelationsToPure,
    hqdmSwapAnyRelationNamesForIds,
    sortOnUuid
    )

import HqdmLib (
    HqdmTriple (..),
    HqdmTriple (subject, predicate, object),
    csvTriplesFromHqdmTriples
    )

import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.List.Split
import Data.Either

main :: IO ()
main = do

    args <- getArgs >>= parse

    let fileList = snd args
    
    if length fileList == 3
        then do
            putStr "\n"
        else do
            hPutStrLn stderr "\n\n\3 Arguments should follow the options in the order <hqdmRelations.csv> <hqdmEntityTypes.csv> <inputFile.csv>\n\n"
            exitWith (ExitFailure 1)

    putStr "**hqdmCardinalityChecker**\n\nLoading model files and the supplied input file of processed mapped User Data.\n"

    let inputRelationsFile = head fileList
    let inputEntityTypeFile = fileList!!1
    let inputFile = fileList!!2

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputEntityTypeFile
    let hqdmInputModel = fromRight [] hqdmTriples

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile inputRelationsFile
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

    pureModelData <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputFile
    let pureModelData = fromRight [] pureModelData

    let outputModel = 
    
    writeFile outputFile ( concat $ csvTriplesFromHqdmTriples outputModel ) 
   
    putStr "\n\n**DONE**\n\n"


------------------------------------------------------------------------------------
-- Argument handling functions
------------------------------------------------------------------------------------

data Flag
    =  Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
   [Option []    ["help"] (NoArg Help)
        "The command should have the general form: hqdmMapToPure hqdmRelations.csv hqdmEntityTypes.csv inputTriples.csv outputTriplesFilename.csv"
   ]

parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute flags argv of
    (args,fs, []) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitSuccess
            else return (nub (concatMap set args), files)

    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: hqdmMapToPure <hqdmRelations.csv> <hqdmEntityTypes.csv> <inputProcessedTriples.csv> <outputFilename.csv>"
          set f      = [f]