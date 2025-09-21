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
-- HqdmCardinalityChecker -spoe hqdmRelations.scv hqdmEntityTypes.csv inputFile.csv 

module Main (main) where

import HqdmRelations (
    HqdmBinaryRelation,
    cardinalityTestAllObjects,
    csvRelationsToPure,
    filterErrorsBy,
    findBrelFromId,
    printableErrorResults,
    rangeTestAllObjects,
    validityFilter
    )

import HqdmLib (
    HqdmTriple (..),
    getSubjects,
    uniqueIds
    )

import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.Either

universalPartBrel::String
universalPartBrel = "7b3caec7-7e9d-47cd-bb19-19d2872c326f"

universalSetBrel::String
universalSetBrel = "2db5490e-01d0-491e-bd64-67ac616f65a0"

universalOrderBrel::String
universalOrderBrel = "cfb37186-d2d6-48de-a418-6197bdf0a7b0"

universalEmergentBrel::String
universalEmergentBrel = "f533fac8-d228-4c10-8799-a26fe6ea16a4"

universalReifiedBrel::String
universalReifiedBrel = "37584690-bff0-493f-80bc-f007af0217fc"

possibleWorldSuperBR::String 
possibleWorldSuperBR = "ac3fd9bd-a64d-4e87-8da6-1ce76451fde5"

possibleWorldBrel::String
possibleWorldBrel = "d2bd9e45-948f-4570-91c2-b0693cd81363"

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

    let inputRelationsFile = head fileList
    let inputEntityTypeFile = fileList!!1
    let inputFile = fileList!!2

    putStr ("**hqdmCardinalityChecker**\n\nLoading model files and the supplied input file of processed mapped User Data, from file '" ++ inputFile ++ "'.\n\nPART1: CARDINALITY VIOLATION CHECKS\n\n")

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputEntityTypeFile
    let hqdmInputModel = fromRight [] hqdmTriples

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile inputRelationsFile
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

    modelData <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputFile
    let pureModelData = fromRight [] modelData

    let uniqueJoinNodes = uniqueIds $ getSubjects pureModelData
    let invalidCardinalityTestResults = validityFilter $ cardinalityTestAllObjects uniqueJoinNodes pureModelData hqdmInputModel relationsInputModel []

    if Parthood `elem` fst args
        then do
            let filteredByPartsResults = filterErrorsBy (head $ findBrelFromId universalPartBrel relationsInputModel) relationsInputModel invalidCardinalityTestResults
            putStr "\n\nPARTHOOD Relation Cardinality Exceptions:\n\n"
            putStr (printableErrorResults filteredByPartsResults hqdmInputModel pureModelData)
            putStr ("\n\n\tNumber of PARTHOOD Relation Cardinality Exceptions:" ++ show (length filteredByPartsResults) ++ "\n\n\n")
        else do
            putStr ""
    
    if Set `elem` fst args
        then do
            let filteredBySetsResults = filterErrorsBy (head $ findBrelFromId universalSetBrel relationsInputModel) relationsInputModel invalidCardinalityTestResults
            putStr "\nSET Relation Cardinality Exceptions:\n\n"
            putStr (printableErrorResults filteredBySetsResults hqdmInputModel pureModelData)
            putStr ("\n\n\tNumber of SET Relation Cardinality Exceptions:" ++ show (length filteredBySetsResults) ++ "\n\n\n")
        else do
            putStr ""
    
    if Order `elem` fst args
        then do
            let filteredByOrderResults = filterErrorsBy (head $ findBrelFromId universalOrderBrel relationsInputModel) relationsInputModel invalidCardinalityTestResults 
            putStr "\nORDER Relation Cardinality Exceptions:\n\n"
            putStr (printableErrorResults filteredByOrderResults hqdmInputModel pureModelData)
            putStr ("\n\n\tNumber of ORDER Relation Cardinality Exceptions:" ++ show (length filteredByOrderResults) ++ "\n\n\n")
        else do
            putStr ""
    
    if Emergent `elem` fst args
        then do
            let filteredByEmergentRelResults = filterErrorsBy (head $ findBrelFromId universalEmergentBrel relationsInputModel) relationsInputModel invalidCardinalityTestResults 
            putStr "\nEMERGENT Relation Cardinality Exceptions:\n\n"
            putStr (printableErrorResults filteredByEmergentRelResults hqdmInputModel pureModelData)
            putStr ("\n\n\tNumber of EMERGENT Relation Cardinality Exceptions:" ++ show (length filteredByEmergentRelResults) ++ "\n\n\n")
        else do
            putStr ""

    if Reified `elem` fst args
        then do
            let filteredByReifiedRelResults = filterErrorsBy (head $ findBrelFromId universalReifiedBrel relationsInputModel) relationsInputModel invalidCardinalityTestResults 
            putStr "\nREIFIED Relation Cardinality Exceptions:\n\n"
            putStr (printableErrorResults filteredByReifiedRelResults hqdmInputModel pureModelData)
            putStr ("\n\n\tNumber of REIFIED Relation Cardinality Exceptions:" ++ show (length filteredByReifiedRelResults) ++ "\n\n\n")
        else do
            putStr ""

    if PossibleWorld `elem` fst args
        then do
            let filteredByPossibleWorldRelResults = filterErrorsBy (head $ findBrelFromId possibleWorldBrel relationsInputModel) relationsInputModel invalidCardinalityTestResults 
            putStr "\nPart_of_possible_world Relation Cardinality Exceptions:\n\n"
            putStr (printableErrorResults filteredByPossibleWorldRelResults hqdmInputModel pureModelData)
            putStr ("\n\n\tNumber of part_of_possible_world Relation Cardinality Exceptions:" ++ show (length filteredByPossibleWorldRelResults))
            putStr "\n\tNote: Strictly, there will always be at least one of these exceptions (see HQDM entity type definition https://github.com/hqdmTop/hqdmFramework/wiki/spatio_temporal_extent)\n\n\n"
        else do
            putStr ""

    putStr ("\n\nTotal number of Relation Cardinality Exceptions:" ++ show (length invalidCardinalityTestResults) ++ "\n\n")

    -- Binary Relation Range Type check
    putStr "\n\nPART2: BINARY RELATION RANGE TYPE VIOLATION CHECKS\n\n"
    let invalidRangeTestResults = validityFilter $ rangeTestAllObjects uniqueJoinNodes pureModelData hqdmInputModel relationsInputModel []
    putStr (printableErrorResults invalidRangeTestResults hqdmInputModel pureModelData)
    putStr ("\n\n\tNumber of Binary Relation Range Exceptions:" ++ show (length invalidRangeTestResults))
    putStr "\n\tNote: Strictly, this only tests for instances of Binary Relations that are present in the dataset.  If an instance of a necessary Binary Relation is missing this will be caught by the Cardinality Checking above.\n\n\n"
   
    putStr "\n\n**DONE**\n\n"


------------------------------------------------------------------------------------
-- Argument handling functions
------------------------------------------------------------------------------------

data Flag
    = Parthood          -- -p
    | Set               -- -s
    | Order             -- -o
    | Emergent          -- -e
    | Reified           -- -r
    | PossibleWorld     -- -w
    | Help              -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
   [Option ['p'] []        (NoArg Parthood)
        "Specifies that the output includes relationships of the Parthood Binary Relation Set."
    ,Option ['s'] []       (NoArg Set)
        "Specifies that the output includes relationships of the Set (HQDM Class) Binary Relation Set."
    ,Option ['o'] []       (NoArg Order)
        "Specifies that the output includes relationships of the Order Binary Relation Set."
    ,Option ['e'] []       (NoArg Emergent)
        "Specifies that the output includes relationships of the Emergent Binary Relation Set."
    ,Option ['r'] []       (NoArg Reified)
        "Specifies that the output includes relationships of the Reified Binary Relation Set."
    ,Option ['w'] []       (NoArg PossibleWorld)
        "Specifies that the output includes relationships of the part_of_possible_world Relation Set."
    ,Option []    ["help"] (NoArg Help)
        "The command should have the general form: hqdmCardinalityChecker -psoerw hqdmRelations.csv hqdmEntityTypes.csv inputTriples.csv"
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

    where header = "Usage: hqdmCardinalityChecker -psoerw <hqdmRelations.csv> <hqdmEntityTypes.csv> <inputProcessedTriples.csv>"
          set f      = [f]