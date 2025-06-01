{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  TimeUtils
-- Description :  Module with functions to apply to handle time as UUIDv1s
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
-- A few of these functions have been copied from Data.Time.UUID,V1 as they
-- are not exported from the package:
-- (https://hackage.haskell.org/package/uuid-1.3.16/docs/src/Data.UUID.V1.html)
-- Credit for them is due to the original authors. 
--
-- A more verbose version of this may be possible without using date-time stamps
-- using 'a' <before> 'b' relations.  This is logically 'better' but would be
-- relation-intensive and would require many relations to be added each time a 
-- new state was introduced to a dataset. 

module TimeUtils (
    TemporalExtentCmp(..),
    PointInTimeTemporalExtentCmp,
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
    ) where

import Data.Bits
import Data.Maybe
import Data.Word
import Data.Time
import Data.UUID
import Data.UUID.Util
import Data.UUID.Types.Internal
import Data.UUID.Types.Internal.Builder
import Network.Info

import HqdmLib (
    HqdmTriple(..),
    HqdmTriple(object),
    lookupHqdmOne
    )

import HqdmQueries (
    filterRelsByAttribute,
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

---------------------------------------------------------------------------------------------------------
-- Functions obtained from: https://hackage.haskell.org/package/uuid-1.3.16/docs/src/Data.UUID.V1.html --
---------------------------------------------------------------------------------------------------------

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

newtype MACSource = MACSource MAC
instance ByteSource MACSource where
    z /-/ (MACSource (MAC a b c d e f)) = z a b c d e f
type instance ByteSink MACSource g = Takes3Bytes (Takes3Bytes g)

---------------------------------------------------------------------------------------------------------
-------------------------------- End of extarnally sourced Functions  -----------------------------------
---------------------------------------------------------------------------------------------------------

utcTimeFromUuid :: String -> UTCTime
utcTimeFromUuid uuid = dt
 where
  hnsgr = fromIntegral $ (fromJust $ extractTime (fromJust $ fromString uuid))
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

----------------------------- BASIC UUIDv1 TIME COMPARISON FUNCTIONS -----------------------------

before :: String -> String -> Bool 
before "" _ = False 
before _ "" = False 
before uid1 uid2 = (utcTimeFromUuid uid1 :: UTCTime) < (utcTimeFromUuid uid2 :: UTCTime)

after :: String -> String -> Bool 
after "" _ = False 
after _ "" = False 
after uid1 uid2 = (utcTimeFromUuid uid1 :: UTCTime) > (utcTimeFromUuid uid2 :: UTCTime)

orderTest :: String -> (UTCTime -> UTCTime -> Bool) -> String -> Bool 
orderTest "" _ _ = False 
orderTest _ _ "" = False 
orderTest uid1 tst uid2 = (utcTimeFromUuid uid1 :: UTCTime) `tst` (utcTimeFromUuid uid2 :: UTCTime)

-- Note, this assumes "" represents an unbounded time (i.e. assumes infinite extent)
between :: String -> String -> String -> Bool 
between "" _ _ = False 
between uidToTest "" b = before uidToTest b
between uidToTest a "" = after uidToTest a
between uidToTest a b = after uidToTest a && before uidToTest b

-- A short-cut to equals would be to test for string match.  However, this wouldn't check the time value.  If a different MAC had been used to 
-- generate each of the uuidv1 values then the string match would fail to resolve times even when they are equal. 
equals :: String -> String -> Bool 
equals "" "" = True -- this assumes "" represents an unbounded time (i.e. assumes infinite extent)
equals "" _ = False 
equals _ "" = False
equals uid1 uid2 = (utcTimeFromUuid uid1 :: UTCTime) == (utcTimeFromUuid uid2 :: UTCTime)

----------------------------- BASIC SPATIO-TEMPORAL STATE COMPARISON FUNCTIONS -----------------------------

-- State and a point in time.  The set of outcomes when testing whether given time value is within the extents of a HQDM state
data PointInTimeTemporalExtentCmp = Before | After | EqStart | EqEnd | During | DuringUnboundedLeft | DuringUnboundedRight | Null deriving (Enum, Show)

-- Expects [0:1] beginning and ending relations in the given Set. 
-- Perhaps add a check that uid is indeed a v1 uuid and that the supplied list of triples is indeed for a single node
pointInTimeCompareWithState :: String -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> PointInTimeTemporalExtentCmp 
pointInTimeCompareWithState "" _ _ _ = Null
pointInTimeCompareWithState _ [] _ _ = Null 
pointInTimeCompareWithState _ _ [] _ = Null 
pointInTimeCompareWithState _ _ _ [] = Null 
pointInTimeCompareWithState uid relSet allRels brels = go uid
    where 
        beginning = headObjectIfTriplePresent $ HqdmQueries.filterRelsByBeginning relSet brels
        ending = headObjectIfTriplePresent $ HqdmQueries.filterRelsByEnding relSet brels
        beginningObject = HqdmLib.lookupHqdmOne beginning allRels 
        endingObject = HqdmLib.lookupHqdmOne ending allRels 
        beginningUuid = headObjectIfTriplePresent $ HqdmQueries.filterRelsByAttribute beginningObject brels
        endingUuid = headObjectIfTriplePresent $ HqdmQueries.filterRelsByAttribute endingObject brels
    
        go uid 
            | before uid beginningUuid = Before
            | after uid endingUuid = After
            | equals uid beginningUuid = EqStart 
            | equals uid endingUuid = EqEnd 
            | (after uid beginningUuid) && (before uid endingUuid) = During 
            | (after uid beginningUuid) && (endingUuid == "") = DuringUnboundedRight
            | (before uid endingUuid) && (beginningUuid == "") = DuringUnboundedLeft
            | otherwise = Null 

-- Full state comparison along the lines of Allens Temporal Algebra (but with qualification of unbounded limits)
-- precedes, overlaps, share start/end/extent, within (both ways round for some of these)
data TemporalExtentCmp = 
      PrecedesSnd               -- Allen's Precedes
    | PrecedesFst 
    | MeetsSnd                  -- Allen's Meets
    | MeetsFst 
    | OverlapsSnd               -- Allen's Overlaps
    | OverlapsFst 
    | StartsSnd                 -- Allen's Starts
    | StartsFst 
    | DuringSnd                 -- Allen's During
    | DuringFst 
    | DuringSndUnbounded        -- -- 
    | DuringFstUnbounded 
    | DuringSndBothUnbounded 
    | DuringFstBothUnbounded 
    | EndsSnd                   -- Allen's Finishes
    | EndsFst 
    | EqualExtent               -- Allen's is equal to.  Temporal extents match.
    | AllenNull                 -- Some input condition is not met (e.g. no temporal bounds present or time format unresolvable)
    deriving (Eq, Enum, Show)

getObjectAttribute :: String -> [HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> String 
getObjectAttribute obj tpls brels = headObjectIfTriplePresent $ HqdmQueries.filterRelsByAttribute (HqdmLib.lookupHqdmOne obj tpls) brels 

isUuidV1 :: String -> Bool
isUuidV1 "" = True  -- Empty string represents unbounded time value
isUuidV1 str = go 
    where
        uuid = fromString str
        go 
            | isNothing uuid = False
            | otherwise = version (fromJust uuid) == 1

-- | temporalOverlapTest
-- Full state temporal-extent overlap test (based on Allen's Interval Agebra BUT also allowing for unbounded states)
-- This is not a parthood test.  That is a relation-only query. 
temporalOverlapTest :: [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmLib.HqdmTriple] -> [HqdmRelations.HqdmBinaryRelationPure] -> TemporalExtentCmp
temporalOverlapTest [] _ _ _ = AllenNull 
temporalOverlapTest _ [] _ _ = AllenNull 
temporalOverlapTest _ _ [] _ = AllenNull 
temporalOverlapTest _ _ _ [] = AllenNull 
temporalOverlapTest state1 state2 tpls brels = go
    where 
        state1begin = headObjectIfTriplePresent $ HqdmQueries.filterRelsByBeginning state1 brels
        state1end = headObjectIfTriplePresent $ HqdmQueries.filterRelsByEnding state1 brels
        state2begin = headObjectIfTriplePresent $ HqdmQueries.filterRelsByBeginning state2 brels
        state2end = headObjectIfTriplePresent $ HqdmQueries.filterRelsByEnding state2 brels

        state1beginUuid = getObjectAttribute state1begin tpls brels 
        state1endUuid = getObjectAttribute state1end tpls brels 
        state2beginUuid = getObjectAttribute state2begin tpls brels
        state2endUuid = getObjectAttribute state2end tpls brels

        v1Test = (isUuidV1 state1beginUuid) && (isUuidV1 state1endUuid) && (isUuidV1 state2beginUuid) && (isUuidV1 state2endUuid)

        go 
            | v1Test == False = AllenNull
            | (state1beginUuid == "" && state1endUuid == "") || (state2beginUuid == "" && state2endUuid == "") = AllenNull
            | (equals state1beginUuid state2beginUuid) && (equals state1endUuid state2endUuid)  = EqualExtent -- This is here to catch it before the StartsSnd and StartsFst.
            | before state1endUuid state2beginUuid                                              = PrecedesSnd
            | before state2endUuid state1beginUuid                                              = PrecedesFst
            | equals state1endUuid state2beginUuid                                              = MeetsSnd
            | equals state2endUuid state1beginUuid                                              = MeetsFst
            | (between state1beginUuid state2beginUuid state2endUuid) && (between state1endUuid state2beginUuid state2endUuid) = DuringSnd
            | (between state2beginUuid state1beginUuid state1endUuid) && (between state2endUuid state1beginUuid state1endUuid) = DuringFst 
            -- |  = DuringSndUnbounded 
            -- |  = DuringFstUnbounded 
            -- |  = DuringSndBothUnbounded 
            -- |  = DuringFstBothUnbounded 
            | (equals state1beginUuid state2beginUuid) && (before state1endUuid state2endUuid)  = StartsSnd 
            | (equals state1beginUuid state2beginUuid) && (after state1endUuid state2endUuid)   = StartsFst -- Not sure if this is a valid outcome when accommodating unbounded states.  Revisit this if it causes issues. 
            | between state1endUuid state2beginUuid state2endUuid                               = OverlapsSnd
            | between state2endUuid state1beginUuid state1endUuid                               = OverlapsFst            
            | (equals state1endUuid state2endUuid) && (before state2beginUuid state1beginUuid)  = EndsSnd
            | (equals state1endUuid state2endUuid) && (after state2beginUuid state1beginUuid)   = EndsFst -- Not sure if this is a valid outcome when accommodating unbounded states.  Revisit this if it causes issues. 
            | otherwise = AllenNull 