{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmRelationInheritance Main
-- Description :  Structure and specification of the Inherited Relation Sets including their 
--                Cardinalities, derived from the supplied HQDM specification files.
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that generates the relation SETs for all HQDM AllAsData.

module Main (main) where

import HqdmRelations (
    HqdmBinaryRelation,
    printRelation,
    findBrelFromId,
    superRelationPathsToUniversalRelation,
    relIdNameTupleLayers,
    csvRelationsToPure,
    printablePathFromTuplesWithDomainAndRange,
    findSubBinaryRelationTree
    )

import HqdmMermaid (
    mermaidAddTitle,
    mermaidSuperRelationPathsToUniversalRelation,
    mermaidTDTopAndTail,
    insertBRNodeName
    )

import HqdmLib (HqdmTriple(..))

import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.Either

main :: IO ()
main = do

    args <- getArgs >>= parse

    let fileList = snd args
    print fileList
    
    if length fileList == 3
        then do
            putStr "\n\n"
        else do
            hPutStrLn stderr "\n\n\3 Arguments should follow the options in the order <Relation_file.csv> <EntityType_file.csv> <uuid_of_relation>\n\n"
            exitWith (ExitFailure 1)

    let inputRelationsFile = head fileList
    let inputEntityTypeFile = fileList!!1
    let relId = fileList!!2

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile inputRelationsFile
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputEntityTypeFile
    let hqdmInputModel = fromRight [] hqdmTriples

    let superBRPathToUniversal = superRelationPathsToUniversalRelation [[relId]] relationsInputModel
    let subBRTree = findSubBinaryRelationTree [[relId]] relationsInputModel
    let speifiedRelationNotPresent = null (findBrelFromId relId relationsInputModel)

    if speifiedRelationNotPresent
        then do
            hPutStrLn stderr "Provided relation uuid is not present in the input file.\n\n"
            exitWith (ExitFailure 1) 
        else do
            return ()

    if Ascii `elem` fst args && not speifiedRelationNotPresent
        then do 
            putStr ("\n\nASCII Relation Inheritance Path To Universal Binary Relation Set from (" ++ relId ++ "):\n\n\n")
            putStr ( printablePathFromTuplesWithDomainAndRange (relIdNameTupleLayers superBRPathToUniversal relationsInputModel) relationsInputModel hqdmInputModel)
            putStr ("\n\nNumber of super-Binary Relation Sets (including the specified the specified entity type): " ++ show (length (concat superBRPathToUniversal)))
            putStr ("\n\nASCII (non-Strict) sub-Relation Inheritance Path From Specified Binary Relation Set (" ++ relId ++ "):\n\n\n")
            putStr ( printablePathFromTuplesWithDomainAndRange (relIdNameTupleLayers (reverse subBRTree) relationsInputModel) relationsInputModel hqdmInputModel)
            putStr ("\n\nNumber of sub-Binary Relation Sets (including the specified the specified entity type): " ++ show (length (concat subBRTree)))
            putStr ( "\n\n\n" ++ printRelation (head $ findBrelFromId (head $ head superBRPathToUniversal) relationsInputModel) )
        else putStr "\n\n"

    if Mermaid `elem` fst args && not speifiedRelationNotPresent
        then do 
            putStr "\n\nMermaid TD graph of the supertypes:\n\n"
            let mmGraph = mermaidAddTitle (mermaidTDTopAndTail (insertBRNodeName relId relationsInputModel ++ mermaidSuperRelationPathsToUniversalRelation [[relId]] relationsInputModel "")) ("Supertype graph for " ++ relId)
            putStr mmGraph
            putStr "\nMermaid graph can be rendered using this online tool: https://mermaid.live/edit\n\n"
        else putStr "\n\n"

------------------------------------------------------------------------------------
-- Argument handling functions
------------------------------------------------------------------------------------

data Flag
    = Ascii                 -- -a
    | Mermaid               -- -m
    | Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
   [Option ['a'] []        (NoArg Ascii)
        "Specifies that the output is in ASCII text."
    ,Option ['m'] []       (NoArg Mermaid)
        "Specifies that the output is in Mermaid form."
    ,Option []    ["help"] (NoArg Help)
        "The command should have the general form: relationPathUp -a PureHqdmRelations_v5.csv HqdmAllAsDataFormal4.csv uuid_of_a_relation"
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

    where header = "Usage: relationPathUp [-am] [relFile] [entityTypeFile] [relUuid]"
          set f      = [f]

