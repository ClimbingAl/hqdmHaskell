{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified TimeUtils (
  after,
  before,
  between,
  equals,
  orderTest,
  headObjectIfTriplePresent,
  pointInTimeCompareWithState,
  temporalOverlapTest,
  utcTimeFromUuid,
  uuidFromUTCTime
  )

import qualified StringUtils (
  addNewEntryIfNotInMap,
  createEmptyUuidMap,
  joinStringsFromMap,
  listRemoveDuplicates,
  stringToDateOrHashUuid,
  stringTuplesFromTriples,
  uuidV5FromString
 )

import qualified HqdmLib ( 
  HqdmTriple(..),
  HqdmTriple(subject, predicate, object),
  lookupHqdmOne,
  nodeIdentityTest )

import HqdmRelations ( 
    HqdmBinaryRelation,
    csvRelationsToPure
  )

import HqdmQueries (
    filterRelsByAttribute,
    filterRelsByBeginning,
    filterRelsByEnding
    )

import qualified Data.Map as Map
import Data.Maybe
import Data.List (nub)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601
import Network.Info
import Data.UUID
import Data.UUID.Util

import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader( NoHeader ), decode)
import qualified Data.Vector as V
import Data.Either

joinModelFilename::String
joinModelFilename = "../hqdmJoin/joinedAllRelsTestStrict.csv"

temporalAlgebraTestFilename::String 
temporalAlgebraTestFilename = "./test/temporalAlgebraMapped.csv"

hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../PureHqdmRelations_v92.csv"

main :: IO ()
main = do
  putStrLn "Experimental Time to uuid1 package."

  hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename
  let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

  putStr "\n\nCreate a fixed MAC address to be used in the generated uuid V1s (0xBB 0x32 0x09 0xDE 0x79 0xC0):\n\n"
  let myHqdmMac = MAC 0xBB 0x32 0x09 0xDE 0x79 0xC0
  print myHqdmMac

  putStr "\n\nCreate a valid ISO8601 dateTime to do round-trip test with: \n\n"
  let dateTime1 = fromJust $ (iso8601ParseM "2021-07-05T14:40:25.4368657Z" :: Maybe UTCTime) -- Only times to 100ns increments are supported.  This is a constraint of uuid Version1
  let dateTime2 = fromJust $ (iso8601ParseM "2022-07-05T14:40:25.4368657Z" :: Maybe UTCTime)
  let dateTime3 = fromJust $ (iso8601ParseM "2026-07-30T17:11:06.752173Z" :: Maybe UTCTime)
  let uuid1 = TimeUtils.uuidFromUTCTime dateTime1
  let uuid2 = TimeUtils.uuidFromUTCTime dateTime2
  let uuid3 = TimeUtils.uuidFromUTCTime dateTime3

  putStr "\n\nTest between function: "
  print (TimeUtils.between uuid2 uuid1 uuid3)

  {- Removed exports from TimeUtils.  This code block is left for reference only. 
  putStr "\n\nCalculate the number of 100ns units singe Gregorian Refore time: \n\n"
  let hnsSinceGregorianReform = TimeUtils.hundredsOfNanosSinceGregorianReform (fromJust dateTime) 
  print hnsSinceGregorianReform

  putStr "\n\nMake the uuid V1 from the time value and MAC: \n\n"
  let myUuid1 = TimeUtils.makeUUID hnsSinceGregorianReform 0x0000 myHqdmMac
  print myUuid1

  let myUuid1_2 = TimeUtils.makeUUID (TimeUtils.hundredsOfNanosSinceGregorianReform (fromJust dateTime2)) 0x0000 myHqdmMac
  print myUuid1_2

  print ("Now compare the uuidV1s: " ++ (show $ TimeUtils.before (toString myUuid1_2) (toString myUuid1)))
  putStr "\n\n"
  print ("Now compare using orderTest: < :" ++ (show $ TimeUtils.orderTest (toString myUuid1) (<=) (toString myUuid1_2)))

  putStr "\n\nRe-calculate the numerber of 100ns Units from the uuid: \n\n"
  let hnsTimeFromUuid1 = extractTime myUuid1
  print hnsTimeFromUuid1

  putStr "\n\nRegenerate the original time from the uuid: \n\n"
  let utcFromUuid = TimeUtils.utcTimeFromUuid myUuid1
  print utcFromUuid
  -}

  putStr "\n\nNow generate a uuidV5 from a String:\n\n"
  let testString = "This is an arbitrary string with a load of £$%^&*1234567890_+-=:@~?><,.// and so on\n\n and more and more"
  let uuid5 = StringUtils.uuidV5FromString testString
  print uuid5

  let uuidV5Map = StringUtils.createEmptyUuidMap
  let uuidV1Map = StringUtils.createEmptyUuidMap
  let uuidV1MapPOSIX = StringUtils.createEmptyUuidMap

  let uuidV5Map1 = snd $ StringUtils.stringToDateOrHashUuid testString (uuidV1Map, uuidV5Map)
  let uuidV1Map1 = fst $ StringUtils.stringToDateOrHashUuid "2021-07-05T14:40:25.4368657Z" (uuidV1Map, uuidV5Map)
  let uuidV1Map1POSIX1 = fst $ StringUtils.stringToDateOrHashUuid "1758412800" (uuidV1MapPOSIX, uuidV5Map) -- 21 Sept 2025 00:00:00hrs
  print (Map.toList uuidV5Map1)
  print (Map.toList uuidV1Map1)
  print (Map.toList uuidV1Map1POSIX1)

  -- Now test the conversion of a mapped dataset
  joinModelTriples <- fmap V.toList . decode @HqdmLib.HqdmTriple NoHeader <$> BL.readFile joinModelFilename
  let joinInputModel = fromRight [] joinModelTriples
  let convertedStrings = StringUtils.listRemoveDuplicates $ StringUtils.stringTuplesFromTriples joinInputModel []

  -- Add it to a Map
  let finalMap = Map.fromList convertedStrings

  -- Now replace the original strings with their uuid keys
  let fullyJoinedInputModel = StringUtils.joinStringsFromMap joinInputModel finalMap
  --putStr "\n\nWrite converted dataset to terminal\n\n"
  --print fullyJoinedInputModel

  -- Fetch the relations for a single HQDM object
  let testId = "9ffa0fc1-3365-4f63-801d-63ef72bda8e1" 
  let testObject = HqdmLib.lookupHqdmOne testId fullyJoinedInputModel
  print testObject

  let cmpResult = TimeUtils.pointInTimeCompareWithState (TimeUtils.uuidFromUTCTime $ TimeUtils.utcTimeFromUuid uuid2) testObject fullyJoinedInputModel relationsInputModel
  putStr "\n\nCompare testUuidv1 with provided state: "
  print cmpResult

  {-
  let b = TimeUtils.headObjectIfTriplePresent $ HqdmQueries.filterRelsByBeginning testObject relationsInputModel
  let beginningObj = HqdmLib.lookupHqdmOne b fullyJoinedInputModel
  let beginningUuid = TimeUtils.headObjectIfTriplePresent $ HqdmQueries.filterRelsByAttribute beginningObj relationsInputModel
  -}

 -- Now trial the temporalAlgebra test dataset and functionj
  temporalAlgebraTestTriples <- fmap V.toList . decode @HqdmLib.HqdmTriple NoHeader <$> BL.readFile temporalAlgebraTestFilename
  let temporalAlgebraTestModel = fromRight [] temporalAlgebraTestTriples
  let convertedTAStrings = StringUtils.listRemoveDuplicates $ StringUtils.stringTuplesFromTriples temporalAlgebraTestModel []
  let finalTAMap = Map.fromList convertedTAStrings
  let fullyJoinedTAInputModel = StringUtils.joinStringsFromMap temporalAlgebraTestModel finalTAMap

  putStr "\n\nNow explore the Temporal Algebra Test Dataset and Function:\n\n"

  -- Fetch the relations for Object A
  let testTAObjectA = HqdmLib.lookupHqdmOne "6dec78f4-4c61-4c60-8bca-22fc280ce997" fullyJoinedTAInputModel
  let testTAObjectB = HqdmLib.lookupHqdmOne "013273a5-8510-4e83-8ed9-2b730978c58e" fullyJoinedTAInputModel
  let testTAObjectC = HqdmLib.lookupHqdmOne "0cc1c062-428e-4cb3-bc43-18165b04c060" fullyJoinedTAInputModel
  let testTAObjectD = HqdmLib.lookupHqdmOne "c9b24228-8361-4a25-889c-8d96ec615e0d" fullyJoinedTAInputModel
  let testTAObjectE = HqdmLib.lookupHqdmOne "97a06964-7fc2-4a57-8460-b34f1f43ea6a" fullyJoinedTAInputModel
  let testTAObjectF = HqdmLib.lookupHqdmOne "5213bd4f-ec37-4040-8316-85af3241e145" fullyJoinedTAInputModel
  let testTAObjectG = HqdmLib.lookupHqdmOne "12b56f76-8024-46d5-9c4e-4657deee9b14" fullyJoinedTAInputModel
  let testTAObjectH = HqdmLib.lookupHqdmOne "2350bc3f-2fb0-4fde-a04a-a83e6ab9ab1c" fullyJoinedTAInputModel
  let testTAObjectI = HqdmLib.lookupHqdmOne "3fd594a9-b112-44db-8b2e-6fb4cf087963" fullyJoinedTAInputModel
  let testTAObjectJ = HqdmLib.lookupHqdmOne "40d24c14-8fef-4257-99b5-77249ee3e139" fullyJoinedTAInputModel
  let testTAObjectK = HqdmLib.lookupHqdmOne "e3c20eb6-e738-4891-83e1-64e6efc87bfd" fullyJoinedTAInputModel
  let testTAObjectZ = HqdmLib.lookupHqdmOne "86a568f0-be92-47e4-af8d-701729331dde" fullyJoinedTAInputModel

  let cmpTAResult = TimeUtils.pointInTimeCompareWithState (TimeUtils.uuidFromUTCTime $ TimeUtils.utcTimeFromUuid uuid3) testTAObjectA fullyJoinedTAInputModel relationsInputModel
  putStr "\n\nCompare uuid3 with provided state: "
  print cmpTAResult

  putStr "\n\nNow try the temporal algebra function: \n\n"
  print ("1  " ++ show (TimeUtils.temporalOverlapTest testTAObjectZ testTAObjectB fullyJoinedTAInputModel relationsInputModel))
  print ("2  " ++ show (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectB fullyJoinedTAInputModel relationsInputModel))
  print ("3  " ++ show (TimeUtils.temporalOverlapTest testTAObjectB testTAObjectA fullyJoinedTAInputModel relationsInputModel))
  print ("4  " ++ show (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectC fullyJoinedTAInputModel relationsInputModel))
  print ("5  " ++ show (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectA fullyJoinedTAInputModel relationsInputModel))
  print ("6  " ++ show (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectB fullyJoinedTAInputModel relationsInputModel))
  print ("7  " ++ show (TimeUtils.temporalOverlapTest testTAObjectB testTAObjectC fullyJoinedTAInputModel relationsInputModel))
  print ("8  " ++ show (TimeUtils.temporalOverlapTest testTAObjectD testTAObjectC fullyJoinedTAInputModel relationsInputModel))
  print ("9  " ++ show (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectD fullyJoinedTAInputModel relationsInputModel))
  print ("10 " ++ show (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectE fullyJoinedTAInputModel relationsInputModel))
  print ("11 " ++ show (TimeUtils.temporalOverlapTest testTAObjectE testTAObjectC fullyJoinedTAInputModel relationsInputModel))
  print ("12 " ++ show (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectF fullyJoinedTAInputModel relationsInputModel)) -- ????
  print ("13 " ++ show (TimeUtils.temporalOverlapTest testTAObjectF testTAObjectC fullyJoinedTAInputModel relationsInputModel)) -- ????
  print ("14 " ++ show (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectG fullyJoinedTAInputModel relationsInputModel))
  print ("15 " ++ show (TimeUtils.temporalOverlapTest testTAObjectG testTAObjectC fullyJoinedTAInputModel relationsInputModel)) -- ????
  print ("16 " ++ show (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectH fullyJoinedTAInputModel relationsInputModel))
  print ("17 " ++ show (TimeUtils.temporalOverlapTest testTAObjectI testTAObjectJ fullyJoinedTAInputModel relationsInputModel))
  print ("18 " ++ show (TimeUtils.temporalOverlapTest testTAObjectK testTAObjectK fullyJoinedTAInputModel relationsInputModel))
  print ("19 " ++ show (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectA fullyJoinedTAInputModel relationsInputModel))
 
  
