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
    stringToDateOrHashUuid,
    stringTuplesFromTriples,
    uuidV5FromString
    ) where

import Data.Bits
import Data.List (nub)
import Data.Maybe
import qualified Data.Map as Map -- Perhaps use StringMap in the future
import Data.Word
import Data.UUID.Types ( toString )
import Data.UUID.V5 ( generateNamed )
import Codec.Binary.UTF8.String ( encode )
import Data.UUID ( UUID (..), nil, toString )
import TimeUtils ( uuidFromUTCTime )
import Data.Time.Format.ISO8601 ( iso8601ParseM )
import Data.Time.Clock
import qualified  HqdmLib ( 
    HqdmTriple(..),
    HqdmTriple(subject, predicate, object),
    nodeIdentityTest )

--unsafeFromString :: String -> UUID
--unsafeFromString = fromJust . fromString

-- Add acknowledgements to the source for these functions
uuidV5FromString :: String -> String
uuidV5FromString str = toString $ generateNamed namespaceUuid (encode str)

-- HQDM Haskell
namespaceUuid = generateNamed nil (encode "https://github.com/ClimbingAl/hqdmHaskell#")

-- Tuple from uuidV1 and uuidV5 and original Strings into a Map
addNewEntryIfNotInMap :: Map.Map String String -> (String, String) -> Map.Map String String
addNewEntryIfNotInMap m t = if Map.member (fst t) m then m else uncurry Map.insert t m

-- Create two new Maps to hold uuidV1 and uuidV5 lookups
createEmptyUuidMap :: Map.Map k a
createEmptyUuidMap = Map.empty

-- If ISO 8601 dateTime string then process as uuidV1 else uuidV5
stringToDateOrHashUuid :: String -> (Map.Map String String, Map.Map String String) -> (Map.Map String String, Map.Map String String)
stringToDateOrHashUuid str uidMaps = go str uidMaps
    where
        dateTime = iso8601ParseM str :: Maybe UTCTime

        go str uidMaps
         | HqdmLib.nodeIdentityTest str = uidMaps -- Defence against re-uuid-'ing anu uuid that is passed to this function
         | isNothing dateTime = ( fst uidMaps, addNewEntryIfNotInMap (snd uidMaps) ( uuidV5FromString str, str ))
         | otherwise = ( addNewEntryIfNotInMap (fst uidMaps) ( TimeUtils.uuidFromUTCTime ( fromJust dateTime ), str ), snd uidMaps )


-- Extract strings from s-p-o triples into list
stringTuplesFromTriples :: [HqdmLib.HqdmTriple] -> [(String, String)] -> [(String, String)]
stringTuplesFromTriples [] tupls = tupls
stringTuplesFromTriples (tpl:tpls) tupls
        | HqdmLib.nodeIdentityTest (HqdmLib.object tpl) = stringTuplesFromTriples tpls tupls
        | isNothing maybeTime = stringTuplesFromTriples tpls (tupls ++ [( uuidV5FromString (HqdmLib.object tpl), (HqdmLib.object tpl) )])
        | otherwise = stringTuplesFromTriples tpls (tupls ++ [( TimeUtils.uuidFromUTCTime ( fromJust maybeTime ), (HqdmLib.object tpl) )])
    where maybeTime = ( iso8601ParseM (HqdmLib.object tpl) :: Maybe UTCTime )

listRemoveDuplicates :: (Eq a) => [(a,a)] -> [(a,a)]
listRemoveDuplicates [] = []
listRemoveDuplicates [b] = [b]
listRemoveDuplicates (x:xs) = nub (if (fst x,snd x) `elem` xs then
        listRemoveDuplicates xs else [x] ++ listRemoveDuplicates xs)

