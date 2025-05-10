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
    hundredsOfNanosSinceGregorianReform,
    makeUUID,
    utcTimeFromUuid
    ) where

import Data.Bits
import Data.Maybe
import Data.Word
import Data.Time
import Data.UUID
import Data.UUID.Util
import Data.UUID.Types.Internal
import Network.Info

import Data.UUID.Types.Internal.Builder

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


newtype MACSource = MACSource MAC
instance ByteSource MACSource where
    z /-/ (MACSource (MAC a b c d e f)) = z a b c d e f
type instance ByteSink MACSource g = Takes3Bytes (Takes3Bytes g)

