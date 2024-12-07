{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  HqdmEntityTypeSpec Main
-- Description :  Displays the structure and specification of the supplied HQDM Entity Type name or uuid
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- Executable Main that implements the command line tool functionality.

module Main (main) where

import HqdmRelations (
    HqdmBinaryRelation,
    printRelation,
    findBrelsFromDomain,
    findBrelFromId,
    superRelationPathsToUniversalRelation,
    relIdNameTupleLayers,
    csvRelationsToPure,
    printablePureRelation,
    printablePathFromTuplesWithDomainAndRange
    )

import HqdmLib

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
    
    if length fileList == 3
        then do
            putStr "\n\n"
        else do
            hPutStrLn stderr "\n\n\3 Arguments should follow the options in the order <Relation_file.csv> <EntityType_file.csv> <uuid_or_name_of_entity_type>\n\n"
            exitWith (ExitFailure 1)

    let inputRelationsFile = head fileList
    let inputEntityTypeFile = fileList!!1

    hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile inputRelationsFile
    let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

    hqdmTriples <- fmap V.toList . decode @HqdmTriple NoHeader <$> BL.readFile inputEntityTypeFile
    let hqdmInputModel = fromRight [] hqdmTriples

    let entityId = uuidOrEntityName (fileList!!2) hqdmInputModel

    let entityObj = lookupHqdmOne entityId  hqdmInputModel
    let entityType = head $ lookupHqdmType entityObj
    let rList = zip [1 .. ] (findBrelsFromDomain entityId relationsInputModel)
    
    let subtypes = lookupSubtypes hqdmInputModel
    let stTree = findSupertypeTree [[entityId]] subtypes
    let subTree = findSubtypeTree [[entityId]] subtypes

    let specifiedEntityTypeNotPresent = null entityObj

    if specifiedEntityTypeNotPresent
        then do
            hPutStrLn stderr "Provided entity type uuid is not present in the input file.\n\n"
            exitWith (ExitFailure 1) 
        else do
            return ()

    if Ascii `elem` fst args && not specifiedEntityTypeNotPresent
        then do 
            putStr ("\n\nASCII Entity Type Inheritance Supertype Path To Thing from '" ++ entityType ++ "' (" ++ entityId ++ "):\n\n\n")
            putStr ( reverse $ drop 303 (reverse $ printableTypeTree (reverse stTree) hqdmInputModel ""))
            putStr ("\n\nASCII Entity Type Inheritance Subtype Path from '" ++ entityType ++ "' (" ++ entityId ++ "):\n\n\n")
            putStr ( reverse $ drop 303 (reverse $ printableTypeTree (init subTree) hqdmInputModel ""))
            putStr "\n\nHQDM Relations expressed as Binary Relation Sets:\n\n"
            putStr  (concatMap (\ x -> show (fst x) ++ " " ++ printRelation (snd x) ++ "\n\n") rList)
        else putStr ""
    
    if Raw `elem` fst args && not specifiedEntityTypeNotPresent
        then do 
            putStr "\n\nHQDM Relations expressed as Raw Pure Binary Relation Sets:\n\n"
            print rList
            putStr "\n\n"
        else putStr ""
    
    if Csv `elem` fst args && not specifiedEntityTypeNotPresent
        then do 
            putStr "\n\nHQDM Relations expressed in CSV form:\n\n"
            putStr (concatMap (\ x -> "\t" ++ printablePureRelation (snd x)) rList)
            putStr "\n\n"
        else putStr ""

    if Mermaid `elem` fst args && not specifiedEntityTypeNotPresent
        then do 
            putStr "\n\nMermaid not yet implemented.\n\n"
        else putStr ""
    
    putStr ("Read about it here: https://github.com/hqdmTop/hqdmFramework/wiki/" ++ entityType ++ "\n\n")

-- | uuidOrEntityName 
uuidOrEntityName :: String -> [HqdmTriple] -> String
uuidOrEntityName ip tpls
    | nodeIdentityTest ip = ip
    | otherwise = head $ lookupHqdmIdFromType tpls ip

------------------------------------------------------------------------------------
-- Argument handling functions
------------------------------------------------------------------------------------

data Flag
    = Ascii                 -- -a
    | Mermaid               -- -m
    | Raw                   -- -r
    | Csv                   -- -c
    | Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
   [Option ['a'] []        (NoArg Ascii)
        "Specifies that the output is in ASCII text."
    ,Option ['m'] []       (NoArg Mermaid)
        "Specifies that the output is in Mermaid form."
    ,Option ['r'] []       (NoArg Raw)
        "Specifies that the output is in Raw form."
    ,Option ['c'] []       (NoArg Csv)
        "Specifies that the output is in Csv form."
    ,Option []    ["help"] (NoArg Help)
        "The command should have the general form: entityTypeSpec -a PureHqdmRelations_v5.csv HqdmAllAsDataFormal2.csv uuid_or_name_of_entity_type"
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

    where header = "Usage: entityTypeSpec [-amrc] [relFile] [entityTypeFile] [uuid_or_name_of_entity_type]"
          set f      = [f]


