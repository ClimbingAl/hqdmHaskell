{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified TimeUtils (
  after,
  before,
  orderTest,
  hundredsOfNanosSinceGregorianReform,
  makeUUID,
  utcTimeFromUuid
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
  nodeIdentityTest )

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

main :: IO ()
main = do
  putStrLn "Experimental Time to uuid1 package."

  putStr "\n\nCreate a fixed MAC address to be used in the generated uuid V1s (0xBB 0x32 0x09 0xDE 0x79 0xC0):\n\n"
  let myHqdmMac = MAC 0xBB 0x32 0x09 0xDE 0x79 0xC0
  print myHqdmMac

  putStr "\n\nCreate a valid ISO8601 dateTime to do round-trip test with: \n\n"
  let dateTime =  iso8601ParseM "2021-07-05T14:40:25.4368657Z" :: Maybe UTCTime -- Only times to 100ns increments are supported.  This is a constraint of uuid Version1
  print dateTime

  let dateTime2 =  iso8601ParseM "2022-07-05T14:40:25.4368657Z" :: Maybe UTCTime

  putStr "\n\nCalculate the number of 100ns units singe Grevorian Refore time: \n\n"
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

  putStr "\n\nNow generate a uuidV5 from a String:\n\n"
  let testString = "This is an arbitrary string with a load of £$%^&*1234567890_+-=:@~?><,.// and so on\n\n and more and more"
  let uuid5 = StringUtils.uuidV5FromString testString
  print uuid5
{-
  let uuidV5Map = StringUtils.createEmptyUuidMap
  let uuidV1Map = StringUtils.createEmptyUuidMap
  let uuidV5Map1 = snd $ StringUtils.stringToDateOrHashUuid testString (uuidV1Map, uuidV5Map)
  let uuidV1Map1 = fst $ StringUtils.stringToDateOrHashUuid "2021-07-05T14:40:25.4368657Z" (uuidV1Map, uuidV5Map)
  print (Map.toList uuidV5Map1)
  print (Map.toList uuidV1Map1)

  -- Now test the conversion of a mapped dataset
  joinModelTriples <- fmap V.toList . decode @HqdmLib.HqdmTriple NoHeader <$> BL.readFile joinModelFilename
  let joinInputModel = fromRight [] joinModelTriples
  let convertedStrings = StringUtils.listRemoveDuplicates $ StringUtils.stringTuplesFromTriples joinInputModel []

  -- Add it to a Map
  let uuidMap = StringUtils.createEmptyUuidMap
  let finalMap = Map.fromList convertedStrings

  putStr "\n\nWrite finalMap of the uuids and theis strings (v1&v5 mixed):\n\n"
  print finalMap

  -- Now replace the original strings with their uuid keys
  let fullyJoinedInputModel = StringUtils.joinStringsFromMap joinInputModel finalMap
  putStr "\n\nWrite converted dataset to terminal\n\n"
  print fullyJoinedInputModel
-}