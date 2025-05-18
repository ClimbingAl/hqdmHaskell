{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  TimeUtils
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


module TimeUtils (
    after,
    before,
    orderTest,
    hundredsOfNanosSinceGregorianReform, -- Remove from export when ready
    makeUUID, -- Remove from export when ready
    utcTimeFromUuid,
    uuidFromUTCTime
    ) where

import Data.Bits
import Data.Maybe
import Data.Word
import Data.Time
import Data.Time.Clock
import Data.UUID
import Data.UUID.Util
import Data.UUID.Types.Internal
import Data.UUID.Types.Internal.Builder
import Network.Info

import HqdmLib (
    HqdmTriple(..),
    HqdmTriple(subject, predicate, object),
    nodeIdentityTest
    )

import HqdmQueries (
    filterRelsByBeginning,
    filterRelsByEnding
    )

import HqdmRelations (
    HqdmBinaryRelationPure
    )

headObjectIfTriplePresent :: [HqdmLib.HqdmTriple] -> String
headObjectIfTriplePresent x
  | not (Prelude.null x)   = HqdmLib.object $ head x
  | otherwise      = ""

-- Add acknowledgements to the source for these functions
makeUUID :: Word64 -> Word16 -> MAC -> UUID
makeUUID time clock mac' =
    buildFromBytes 1 /-/ tLow /-/ tMid /-/ tHigh /-/ clock /-/ MACSource mac'
    where tLow = fromIntegral time :: Word32
          tMid = fromIntegral (time `shiftR` 32) :: Word16
          tHigh = fromIntegral (time `shiftR` 48) :: Word16

-- uuidv1 needs the time in hundreds of ns from 00:00hrs 15 October 1582
hundredsOfNanosSinceGregorianReform :: UTCTime -> Word64
hundredsOfNanosSinceGregorianReform t = floor $ 10000000 * dt
 where
  gregorianReform = UTCTime (fromGregorian 1582 10 15) 0
  dt = t `diffUTCTime` gregorianReform

utcTimeFromUuid :: UUID -> UTCTime
utcTimeFromUuid uuid = dt
 where
  hnsgr = fromIntegral $ (fromJust $ extractTime uuid)
  gregorianReform = UTCTime (fromGregorian 1582 10 15) 0
  dt = ((hnsgr / 10000000) :: NominalDiffTime) `addUTCTime` gregorianReform

{- Some random MAC addresses.  Last one chosen for this project.
a3:15:e2:19:fe:2b
b3:39:d6:53:cc:9e
17:12:7b:b2:4c:cd
f4:ec:22:44:d9:43
bb:32:09:de:79:c0
-}
hqdmHaskellMac = MAC 0xBB 0x32 0x09 0xDE 0x79 0xC0

-- uuidFromUTCTime
-- Unsafe. Only use if it is passed a Just
uuidFromUTCTime :: UTCTime -> String
uuidFromUTCTime t = toString $ makeUUID (hundredsOfNanosSinceGregorianReform t) 0x0000 hqdmHaskellMac


newtype MACSource = MACSource MAC
instance ByteSource MACSource where
    z /-/ (MACSource (MAC a b c d e f)) = z a b c d e f
type instance ByteSink MACSource g = Takes3Bytes (Takes3Bytes g)

----------------------------- BASIC UUIDv1 TIME COMPARISON FUNCTIONS -----------------------------

before :: String -> String -> Bool 
before uid1 uid2 = (utcTimeFromUuid (fromJust $ fromString uid1) :: UTCTime) < (utcTimeFromUuid (fromJust $ fromString uid2) :: UTCTime)

after :: String -> String -> Bool 
after uid1 uid2 = (utcTimeFromUuid (fromJust $ fromString uid1) :: UTCTime) > (utcTimeFromUuid (fromJust $ fromString uid2) :: UTCTime)

orderTest :: String -> (UTCTime -> UTCTime -> Bool) -> String -> Bool 
orderTest uid1 tst uid2 = (utcTimeFromUuid (fromJust $ fromString uid1) :: UTCTime) `tst` (utcTimeFromUuid (fromJust $ fromString uid2) :: UTCTime)

between :: String -> String -> String -> Bool 
between uidToTest a b = before a uidToTest && after b uidToTest 

-- A short-cut to equals would be to test for string match.  However, this wouldn't check the time value.  If a different MAC had been used to 
-- generate each of the uuidv1 values then the string match would fail to resolve times even when they are equal. 
equals :: String -> String -> Bool 
equals uid1 uid2 = (utcTimeFromUuid (fromJust $ fromString uid1) :: UTCTime) == (utcTimeFromUuid (fromJust $ fromString uid2) :: UTCTime)

----------------------------- BASIC SPATIO-TEMPORAL STATE COMPARISON FUNCTIONS -----------------------------

-- State and a point in time.  The set of outcomes when testing whether given time value is within the extents of a HQDM state
data PointInTimeTemporalExtentCmp = Before | After | EqStart | EqEnd | During | DuringUnboundedLeft | DuringUnboundedRight | Null deriving (Enum)

-- Expects [0:1] beginning and ending relations in the given Set. 
-- Perhaps add a check that uid is indeed a v1 uuid and that the supplied list of triples is indeed for a single node
pointInTimeCompareWithState :: String -> [HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> PointInTimeTemporalExtentCmp
pointInTimeCompareWithState uid relSet brels = go uid relSet brels
    where 
        beginning = headObjectIfTriplePresent $ HqdmQueries.filterRelsByBeginning relSet brels
        ending = headObjectIfTriplePresent $ HqdmQueries.filterRelsByEnding relSet brels
    
        go uid relSet brels
            | before uid beginning = Before
            | after uid ending = After
            | equals uid beginning = EqStart 
            | equals uid ending = EqEnd 
            | (after uid beginning) && (before uid ending) = During 
            | (after uid beginning) && (ending == "") = DuringUnboundedRight
            | (before uid ending) && (beginning == "") = DuringUnboundedLeft
            | otherwise = Null

-- Full state comparison along the lines of Allens Temporal Algebra (but with qualification of unbounded limits)
-- precedes, overlaps, share start/end/extent, within (both ways round for some of these)
