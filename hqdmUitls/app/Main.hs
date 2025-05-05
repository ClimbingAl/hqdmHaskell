module Main where

import qualified TimeUtils (
  hundredsOfNanosSinceGregorianReform,
  makeUUID,
  utcTimeFromUuid
  )

import Data.Maybe
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601
import Network.Info
import Data.UUID.Util

main :: IO ()
main = do
  putStrLn "Experimental Time to uuid1 package."

  putStr "\n\nCreate a fixed MAC address to be used in the generated uuid V1s (0xBB 0x32 0x09 0xDE 0x79 0xC0):\n\n"
  let myHqdmMac = MAC 0xBB 0x32 0x09 0xDE 0x79 0xC0
  print myHqdmMac

  putStr "\n\nCreate a valid ISO8601 dateTime to do round-trip test with: \n\n"
  let dateTime =  iso8601ParseM "2021-07-05T14:40:25.4368657Z" :: Maybe UTCTime -- Only times to 100ns increments are supported.  This is a constraint of uuid Version1
  print dateTime

  putStr "\n\nCalculate the number of 100ns units singe Grevorian Refore time: \n\n"
  let hnsSingeGregorianReform = TimeUtils.hundredsOfNanosSinceGregorianReform (fromJust dateTime) -- Don't use fromJust beyond this illustration
  print hnsSingeGregorianReform

  putStr "\n\nMake the uuid V1 from the time value and MAC: \n\n"
  let myUuid1 = TimeUtils.makeUUID hnsSingeGregorianReform 0x0000 myHqdmMac
  print myUuid1

  putStr "\n\nRe-calculate the numerber of 100ns Units from the uuid: \n\n"
  let hnsTimeFromUuid1 = extractTime myUuid1
  print hnsTimeFromUuid1

  putStr "\n\nRegenerate the original time from the uuid: \n\n"
  let utcFromUuid = TimeUtils.utcTimeFromUuid myUuid1
  print utcFromUuid