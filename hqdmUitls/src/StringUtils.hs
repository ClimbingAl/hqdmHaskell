-- Truncate time (valid) strings
-- Convert string to uuidV5s
-- Create / load a list of uuid<->strings


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
    uuidV5FromString
    ) where

import Data.Bits
import Data.Maybe
import qualified Data.Map as Map -- Perhaps use StringMap in the future
import Data.Word
import Data.UUID.Types
import Data.UUID.V5 ( generateNamed )
import Codec.Binary.UTF8.String ( encode )
import Data.UUID ( UUID (..), nil, toString )

--unsafeFromString :: String -> UUID
--unsafeFromString = fromJust . fromString

-- Add acknowledgements to the source for these functions
uuidV5FromString :: String -> String
uuidV5FromString str = toString $ generateNamed namespaceUuid (encode str)

-- HQDM Haskell
namespaceUuid = generateNamed nil (encode "https://github.com/ClimbingAl/hqdmHaskell")

-- Tuple from uuidV1 and uuidV5 and original Strings into a Map
addNewEntryIfNotInMap :: Map.Map String String -> (String, String) -> Map.Map String String
addNewEntryIfNotInMap m t =  Map.insert (fst t) (snd t) m

-- Create two new Maps to hold uuidV1 and uuidV5 lookups
createEmptyUuidMap :: Map.Map k a
createEmptyUuidMap = Map.empty