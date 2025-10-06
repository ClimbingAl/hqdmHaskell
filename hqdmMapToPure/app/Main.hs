{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmMapToPure Main
-- Description :  Maps a CSV dataset of Magma Core or Activity Modeller
--                Triples to a pure hqdmHaskell dataset and writes to file
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that implements the command line tool functionality.
--  hqdmMapToPure hqdmRelations.csv hqdmEntityTypes.csv inputFile.csv 
--      outputFile.csv stringMapFile.csv (the last argument is optional)
--
-- This function converts all RDF triples (S,p,o) to strict binary relation
-- pairs that are identifiers ONLY (as uuids).  Where there is a string
-- literal in an input object (o), either a ISO8601 date-time string or 
-- an arbitrary string, they are converted to the appropriate uuid (1 or 5
-- respectively) and the resulting map (as a list of unique tuple pairs) 
-- is exported along with the (pure) map of the binary relation pairs as 
-- a list of [uuid, uuid, uuid].

module Main (main) where

import HqdmRelations (
    HqdmBinaryRelation,
    csvRelationsToPure,
    getRelationNameFromRels,
    hqdmSwapAnyRelationNamesForIdsStrict,
    sortOnUuid,
    subtypesOfFilter
    )

import HqdmLib (
    HqdmTriple (..),
    HqdmTriple (subject, predicate, object),
    csvTriplesFromHqdmTriples,
    getSubjects,
    lookupHqdmIdsFromTypePredicates,
    lookupHqdmOne,
    lookupSubtypes,
    uniqueIds
    )

import HqdmIds

import StringUtils (
    joinStringsFromMap,
    listRemoveDuplicates,
    uuidV5FromString,
    stringTuplesFromTriples
    )

import TimeUtils (
    generateOrderRelations,
    headObjectIfTriplePresent,
    uuidV1Sort
    )

import qualified Data.Map as Map 
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import System.Console.GetOpt
--import System.Directory
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.List.Split
import Data.Either

elementOfType::String
elementOfType = "8130458f-ae96-4ab3-89b9-21f06a2aac78"

hasSuperclass::String
hasSuperclass = "7d11b956-0014-43be-9a3e-f89e2b31ec4f"

main :: IO ()
main = do

    args <- getArgs >>= parse

    let fileList = snd args

    if (length fileList == 4) || (length fileList == 5)
        then do
            putStr "\n\n"
        else do
            hPutStrLn stderr "\n\n\4 Arguments should follow the options in the \
            \order <hqdmRelations.csv> <hqdmEntityTypes.csv> <inputFile.csv> \
            \<outputFile.csv> <stringMapFile.csv> (The last argument is optional)\n\n"
            exitWith (ExitFailure 1)

    let stringMapFile = constructStringMapFilename fileList

    putStr "**hqdmMapToPure**\n\nLoading model files and the supplied input file of processed Triples.\n"

    let inputRelationsFile = head fileList
    let inputEntityTypeFile = fileList!!1
    let inputFile = fileList!!2
    let outputFile = fileList!!3

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputEntityTypeFile
    let hqdmInputModel = fromRight [] hqdmTriples

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile inputRelationsFile
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

    triplesToMap <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputFile
    let hqdmModelToMap = fromRight [] triplesToMap

    -- This allows a Master Map to be submitted and added to.  This file is added to (by overwriting it 
    -- with the consolodated input Map and the newly generated Map).
    inputStringsMap <- loadTupleMap stringMapFile

    putStr "Now strip IRI path parts and map to pure ids.\n"
    
    let joinInputModel = removeIriPathsFromAll hqdmModelToMap

    let uniqueJoinNodes = uniqueIds $ getSubjects joinInputModel
    let nodeTypeStatements = fmap (\ x -> head $ lookupHqdmOne x joinInputModel) uniqueJoinNodes
    let typeIdsOfJoinObjects = 
            zip uniqueJoinNodes (fmap (head . lookupHqdmIdsFromTypePredicates hqdmInputModel . object)  nodeTypeStatements)

    let subtypes = lookupSubtypes hqdmInputModel
    let onlySubtypesOfSte = subtypesOfFilter typeIdsOfJoinObjects spatio_temporal_extent subtypes
    let elementOfTypeName = getRelationNameFromRels elementOfType relationsInputModel
    let elementOfTypeTriples = fmap (\ x -> HqdmTriple (fst x) elementOfTypeName (snd x)) onlySubtypesOfSte
    
    let onlySubtypesOfClass = subtypesOfFilter typeIdsOfJoinObjects hqdmClass subtypes
    let hasSuperClassName = getRelationNameFromRels hasSuperclass relationsInputModel
    let hasSuperclassTriples = fmap (\ x -> HqdmTriple (fst x) hasSuperClassName (snd x)) onlySubtypesOfClass
    
    let joinedResults = sortOnUuid $ joinInputModel ++ hasSuperclassTriples ++ elementOfTypeTriples
    
    let joinedResultsAllIds =  
            hqdmSwapAnyRelationNamesForIdsStrict joinedResults hqdmInputModel relationsInputModel

    let finalMap = (StringUtils.listRemoveDuplicates $ 
            (inputStringsMap ++ StringUtils.stringTuplesFromTriples joinedResultsAllIds []))
    let fullyJoinedInputModel = StringUtils.joinStringsFromMap joinedResultsAllIds (Map.fromList finalMap)

    writeFile outputFile ( concat $ csvTriplesFromHqdmTriples fullyJoinedInputModel )
    writeFile ( stringMapFile ) ( concatMap (\ x -> (fst x) ++ "," ++ (snd x) ++ "\n") finalMap ) 
       
    putStr "\n\nExport to file output file complete.\n\n**DONE**\n\n"

removeIriPathsFromAll :: [HqdmTriple] -> [HqdmTriple]
removeIriPathsFromAll tpls = 
    [ HqdmTriple (removeIriPathIfPresent (subject values)) (removeIriPathIfPresent (predicate values)) 
        (removeIriPathIfPresent (object values)) | values <- tpls ]

removeIriPathIfPresent :: String -> String 
removeIriPathIfPresent str 
    | elem '#' str && isInfixOf "://" str = last (splitOn "#" str)
    | otherwise = str

constructStringMapFilename :: [String] -> String 
constructStringMapFilename args 
    | length args == 4 = "stringMap_" ++ args!!3
    | otherwise = args!!4

{-openStringMapIfFileExists :: String -> IO [(String, String)]
openStringMapIfFileExists fileName
        | not (doesFileExist fileName) = []
        | otherwise = do
            loadTupleMap fileName-}

loadTupleMap :: String -> IO [(String, String)]
loadTupleMap fileName =
    do 
        inputMap <- fmap V.toList . decode @(String, String) NoHeader <$> BL.readFile fileName 
        let tupleMap = fromRight [] inputMap
        return tupleMap
-------------- Add error handling above


------------------------------------------------------------------------------------
-- Argument handling functions
------------------------------------------------------------------------------------

data Flag
    =  Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
   [Option []    ["help"] (NoArg Help)
        "The command should have the general form: hqdmMapToPure hqdmRelations.csv \
        \hqdmEntityTypes.csv inputTriples.csv outputTriplesFilename.csv \
        \[OPTIONAL]masterUuidStringMap.csv\nNote: [OPTIONAL] means that that \
        \argument doesn't need to be supplied.  It allows a master [uuid, \
        \string] map to be added to."
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

    where header = "Usage: hqdmMapToPure <hqdmRelations.csv> <hqdmEntityTypes.csv> \
    \<inputProcessedTriples.csv> <outputFilename.csv> [OPTIONAL]<masterUuidStringMap.csv>"
          set f      = [f]
