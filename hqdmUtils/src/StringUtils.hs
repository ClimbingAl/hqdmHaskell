-- |
-- Module      :  StringUtils
-- Description :  Module with functions to apply to handle time as UUIDv1
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- These functions have been copied from Data.Time.UUID,V1 as they
-- are not exported from the package:
-- (https://hackage.haskell.org/package/uuid-1.3.16/docs/src/Data.UUID.V1.html)


module StringUtils (
    addNewEntryIfNotInMap,
    createEmptyUuidMap,
    listRemoveDuplicates,
    lookupValueFromDateOrHashUuid,
    reverseLookupDateOrHashUuid,
    stringToDateOrHashUuid,
    stringToDateOrHashUuid',
    uuidV5FromString,
    uuidV5StringTest,
    joinStringsFromMap,
    stringTuplesFromTriples
    ) where

import Data.List (nub, find)
import Data.Maybe
import qualified Data.Map as Map -- Perhaps use StringMap in the future
import Data.UUID.V5 ( generateNamed )
import Codec.Binary.UTF8.String ( encode )
import Data.UUID ( UUID, nil, fromString, toString )
import Data.UUID.Util (version)
import TimeUtils ( uuidFromUTCTime )
import Text.Read ( readMaybe )
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC) 
import Data.Time.Format.ISO8601 ( iso8601ParseM )
import Data.Time.Clock
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import qualified  HqdmLib ( nodeIdentityTest, HqdmRDFTriple(..) )

--unsafeFromString :: String -> UUID
--unsafeFromString = fromJust . fromString

-- Add acknowledgements to the source for these functions

uuidV5FromString :: String -> UUID
uuidV5FromString str = generateNamed namespaceUuid (encode str)

-- HQDM Haskell
namespaceUuid = generateNamed nil (encode "https://github.com/ClimbingAl/hqdmHaskell#")

-- Tuple from uuidV1 and uuidV5 and original Strings into a Map
addNewEntryIfNotInMap :: Map.Map UUID String -> (UUID, String) -> Map.Map UUID String
addNewEntryIfNotInMap m t = if Map.member (fst t) m then m else uncurry Map.insert t m

-- Create Map to hold uuidV1 and uuidV5 lookups
createEmptyUuidMap :: Map.Map k a
createEmptyUuidMap = Map.empty

-- If ISO 8601 dateTime string then process as uuidV1, or if an Int treat as a POSIX time, else uuidV5
stringToDateOrHashUuid :: String -> (Map.Map UUID String, Map.Map UUID String) -> (Map.Map UUID String, Map.Map UUID String)
stringToDateOrHashUuid str uidMaps = go str uidMaps
    where
        -- Parse to ZonedTime first, which supports both 'Z' and '+01:00' offsets safely
        zonedTime = iso8601ParseM str :: Maybe ZonedTime
        -- Extract UTCTime out of the successful parse structure
        dateTime  = zonedTimeToUTC <$> zonedTime 
        unixTimeInt = readMaybe str

        go str uidMaps
         | HqdmLib.nodeIdentityTest str = uidMaps -- Defence against re-uuid-'ing a uuid that is passed to this function
         | isJust unixTimeInt = ( addNewEntryIfNotInMap (fst uidMaps) ( TimeUtils.uuidFromUTCTime ( posixSecondsToUTCTime $ fromIntegral (fromJust unixTimeInt) ), str ),
             snd uidMaps )
         | isNothing dateTime = ( fst uidMaps, addNewEntryIfNotInMap (snd uidMaps) ( uuidV5FromString str, str ))
         | otherwise = ( addNewEntryIfNotInMap (fst uidMaps) ( TimeUtils.uuidFromUTCTime ( fromJust dateTime ), str ), snd uidMaps )

stringToDateOrHashUuid' :: String -> Map.Map UUID String -> Map.Map UUID String
stringToDateOrHashUuid' str uidMap = go str uidMap
    where
        -- Parse to ZonedTime first, which supports both 'Z' and '+01:00' offsets safely
        zonedTime = iso8601ParseM str :: Maybe ZonedTime
        -- Extract UTCTime out of the successful parse structure
        dateTime  = zonedTimeToUTC <$> zonedTime 
        unixTimeInt = readMaybe str

        go str uidMap
         | HqdmLib.nodeIdentityTest str = uidMap -- Defence against re-uuid-'ing a uuid that is passed to this function
         | isJust unixTimeInt = addNewEntryIfNotInMap uidMap ( TimeUtils.uuidFromUTCTime ( posixSecondsToUTCTime $ fromIntegral (fromJust unixTimeInt) ), str )
         | isNothing dateTime = addNewEntryIfNotInMap uidMap (uuidV5FromString str, str )
         | otherwise = addNewEntryIfNotInMap uidMap ( TimeUtils.uuidFromUTCTime ( fromJust dateTime ), str )

lookupValueFromDateOrHashUuid :: UUID -> Map.Map UUID String -> String
lookupValueFromDateOrHashUuid key uidMap =
    case Map.lookup key uidMap of
        Just val -> fromMaybe "" (Just val)
        Nothing  -> ""

reverseLookupDateOrHashUuid :: String -> Map.Map UUID String -> UUID
reverseLookupDateOrHashUuid val uidMap =
    fromMaybe nil (findKey val uidMap)
  where
    findKey target m = fst <$> find (\(_, v) -> v == target) (Map.toList m)


-- Extract strings from s-p-o triples into list
stringTuplesFromTriples :: [HqdmLib.HqdmRDFTriple] -> Map.Map UUID String -> Map.Map UUID String
stringTuplesFromTriples [] tupls = tupls
stringTuplesFromTriples (tpl:tpls) tupls
    | HqdmLib.nodeIdentityTest domainObj = stringTuplesFromTriples tpls tupls
    | isJust unixTimeInt                 = stringTuplesFromTriples tpls (Map.insert uuidFromUnix domainObj tupls)
    | isNothing maybeTime                = stringTuplesFromTriples tpls (Map.insert uuidFromStr domainObj tupls)
    | otherwise                          = stringTuplesFromTriples tpls (Map.insert uuidFromTime domainObj tupls)
  where
    domainObj    = HqdmLib.obj tpl
    maybeTime    = iso8601ParseM domainObj :: Maybe UTCTime
    unixTimeInt  = readMaybe domainObj :: Maybe Int
    
    uuidFromUnix = TimeUtils.uuidFromUTCTime (posixSecondsToUTCTime $ fromIntegral (fromJust unixTimeInt))
    uuidFromStr  = uuidV5FromString domainObj
    uuidFromTime = TimeUtils.uuidFromUTCTime (fromJust maybeTime)

listRemoveDuplicates :: (Eq a) => [(a,a)] -> [(a,a)]
listRemoveDuplicates [] = []
listRemoveDuplicates [b] = [b]
listRemoveDuplicates (x:xs) = nub (if (fst x,snd x) `elem` xs then
        listRemoveDuplicates xs else [x] ++ listRemoveDuplicates xs)

-- Replace strings in joinModel from Map
joinStringsFromMap :: [HqdmLib.HqdmRDFTriple] -> Map.Map UUID String -> [HqdmLib.HqdmRDFTriple]
joinStringsFromMap [] _ = []
joinStringsFromMap (tpl:tpls) strMap
        | isUuid = tpl : joinStringsFromMap tpls strMap
        | otherwise = HqdmLib.HqdmRDFTriple (HqdmLib.sub tpl) (HqdmLib.pred tpl) (toString $ head val) : joinStringsFromMap tpls strMap
    where
        isUuid = HqdmLib.nodeIdentityTest (HqdmLib.obj tpl)
        val = lookupKey (HqdmLib.obj tpl) strMap

-- Obtained from:
-- https://stackoverflow.com/questions/58263235/find-a-key-by-having-its-value-using-data-map-in-haskell
lookupKey :: Eq v => v -> Map.Map k v -> [k]
lookupKey val = Map.foldrWithKey go [] where
  go key value found =
    if value == val
    then key:found
    else found

uuidV5StringTest :: String -> Bool
uuidV5StringTest "" = False
uuidV5StringTest str = go
    where
        uuid = Data.UUID.fromString str
        go
            | isNothing uuid = False
            | otherwise = Data.UUID.Util.version (fromJust uuid) == 5
