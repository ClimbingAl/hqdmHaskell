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
    getRelationNameFromRels,
    hqdmSwapAnyRelationNamesForIdsStrict,
    sortOnUuid,
    subtypesOfFilter,
    sortOnStringUuid
    )

import HqdmLib (
    HqdmTriple (..),
    HqdmTriple (subject, predicate, object),
    HqdmRDFTriple (sub, pred, obj, HqdmRDFTriple),
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
    stringTuplesFromTriples,
    createEmptyUuidMap
    )

import System.FilePath (takeFileName, splitFileName, replaceFileName)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader( NoHeader ), decode)
import Data.UUID ( UUID, nil, fromString, toString )
import qualified Data.Vector as V
import System.Console.GetOpt
import System.Directory
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.List.Split
import Data.Either
import Data.Maybe
import qualified Data.IntMap as IntMap

elementOfType::UUID
elementOfType = fromJust $ fromString "8130458f-ae96-4ab3-89b9-21f06a2aac78"

hasSuperclassId::UUID
hasSuperclassId = fromJust $ fromString "7d11b956-0014-43be-9a3e-f89e2b31ec4f"

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

    let inputRelationsFilePath = head fileList
    let inputEntityTypeFilePath = fileList!!1
    let inputFile = fileList!!2
    let outputFile = fileList!!3

    -- Load HqdmAllAsData
    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputEntityTypeFilePath
    let hqdmInputModel = fromRight [] hqdmTriples

    hqdmTypeNames <- fmap V.toList . decode @(UUID, String) NoHeader <$> BL.readFile (replaceFileName inputEntityTypeFilePath ("stringMap_" ++ (takeFileName (inputEntityTypeFilePath))))
    let hqdmStringMap = Map.fromList (fromRight [] hqdmTypeNames)

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile inputRelationsFilePath
    let relationsInputModel = fromRight [] hqdmRelationSets

    triplesToMap <- fmap V.toList . decode @HqdmRDFTriple NoHeader <$> BL.readFile inputFile
    let hqdmModelToMap = fromRight [] triplesToMap

    -- This allows a Master Map to be submitted and added to.  This file is added to (by overwriting it 
    -- with the consolodated input Map and the newly generated Map).
    inputStringsMap <- openStringMapIfFileExists stringMapFile

    putStr "Now strip IRI path parts and map to pure ids.\n"

    let joinInputModel = removeIriPathsFromAll hqdmModelToMap -- Still [HqdmRDFTriple]

    let nodeTypeStatements = [values | values <- joinInputModel, HqdmLib.pred values == "type"]
    let typeUUIDTuplesOfJoinObjects = map (\x -> (fromJust (fromString $ HqdmLib.sub x), head $ lookupHqdmIdsFromTypePredicates hqdmInputModel (fromJust $ lookupUUID (HqdmLib.obj x) hqdmStringMap))) nodeTypeStatements
    
    let subtypes = lookupSubtypes hqdmInputModel
    let onlySubtypesOfSte = subtypesOfFilter typeUUIDTuplesOfJoinObjects spatio_temporal_extent subtypes
    let elementOfTypeName = getRelationNameFromRels elementOfType relationsInputModel
    let elementOfTypeTriples = fmap (\ x -> HqdmRDFTriple (toString $ fst x) elementOfTypeName (toString $ snd x)) onlySubtypesOfSte

    let onlySubtypesOfClass = subtypesOfFilter typeUUIDTuplesOfJoinObjects hqdmClass subtypes
    let hasSuperClassName = getRelationNameFromRels hasSuperclassId relationsInputModel
    let hasSuperclassTriples = fmap (\ x -> HqdmRDFTriple (toString $ fst x) hasSuperClassName (toString $ snd x)) onlySubtypesOfClass

    let stringMapForOutput = Map.union (Map.fromList inputStringsMap) (StringUtils.stringTuplesFromTriples joinInputModel createEmptyUuidMap)
    let combinedStringMaps = Map.union hqdmStringMap stringMapForOutput

    let joinedResults = sortOnStringUuid (joinInputModel ++ hasSuperclassTriples ++ elementOfTypeTriples)
    let mappedPredicatestoUuids = StringUtils.joinStringsFromMap joinedResults combinedStringMaps 
    
    let joinedResultsAllIds =
            hqdmSwapAnyRelationNamesForIdsStrict mappedPredicatestoUuids hqdmInputModel relationsInputModel

    writeFile outputFile (concat $ csvTriplesFromHqdmTriples joinedResultsAllIds)
    writeFile stringMapFile (concatMap (\(uuid, val) -> show uuid ++ "," ++ val ++ "\n") (Map.toList stringMapForOutput))

    putStr "\n\nExport to output files complete.\n\n**DONE**\n\n"

removeIriPathsFromAll :: [HqdmRDFTriple] -> [HqdmRDFTriple]
removeIriPathsFromAll tpls =
    [ HqdmRDFTriple (removeIriPathIfPresent (HqdmLib.sub values)) (removeIriPathIfPresent (HqdmLib.pred values))
        (removeIriPathIfPresent (HqdmLib.obj values)) | values <- tpls ]

removeIriPathIfPresent :: String -> String
removeIriPathIfPresent str
    | elem '#' str && isInfixOf "://" str = last (splitOn "#" str)
    | otherwise = str

constructStringMapFilename :: [String] -> String
constructStringMapFilename args
    | length args == 4 = "stringMap_" ++ takeFileName (args !! 3)
    | otherwise = args!!4

openStringMapIfFileExists :: String -> IO [(UUID, String)]
openStringMapIfFileExists fileName = do
    x <- doesFileExist fileName
    if not x
        then return []
        else do loadTupleMap fileName

loadTupleMap :: String -> IO [(UUID, String)]
loadTupleMap fileName =
    do
        inputMap <- fmap V.toList . decode @(UUID, String) NoHeader <$> BL.readFile fileName
        let tupleMap = fromRight [] inputMap
        return tupleMap

lookupUUID :: String -> Map.Map UUID String -> Maybe UUID
lookupUUID targetVal m =
    fmap fst $ find (\(_, val) -> val == targetVal) (Map.toList m)
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
        \[OPTIONAL]stringMap_outputTriplesFilename.csv\n\nNote: [OPTIONAL] means that that \
        \argument doesn't need to be supplied.  It allows a master [uuid, \
        \string] map to be added to, as long as it has the prescribed filename form.\n\n\
        \It is also expected that the stringMap of the hqdmEntityTypes.csv is also present."
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
