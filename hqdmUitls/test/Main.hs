module Main (main) where

import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

import qualified TimeUtils (
  after,
  before,
  orderTest,
  hundredsOfNanosSinceGregorianReform,
  headObjectIfTriplePresent,
  makeUUID,
  pointInTimeCompareWithState,
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

main :: IO ()
main = hspec simpleStringToUUIDv1Spec

simpleStringToUUIDv1Spec :: Spec
simpleStringToUUIDv1Spec = describe "Tests of a basic TimeUtils function" $ do
  let dateTime =  iso8601ParseM "2021-07-05T14:40:25.4368657Z" :: Maybe UTCTime
  context "A known ISO8601 date-time string of 2021-07-05T14:40:25.4368657Z should convert to a the uuidV1 of ef4dfb91-dd9e-11eb-8000-bb3209de79c0" $
    it "It should match: " $
      TimeUtils.uuidFromUTCTime (fromJust dateTime) `shouldBe` "ef4dfb91-dd9e-11eb-8000-bb3209de79c0"
