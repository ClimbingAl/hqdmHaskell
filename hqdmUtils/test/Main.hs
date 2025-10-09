{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

import qualified TimeUtils (
  TemporalExtentCmp(..),
  after,
  before,
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

temporalAlgebraTestFilename::String 
temporalAlgebraTestFilename = "./test/temporalAlgebraMapped.csv"

hqdmRelationsInputFilename::String
hqdmRelationsInputFilename = "../HqdmBinaryRelations_v4.csv"

main :: IO ()
main = do 

  hqdmRelationSets <- fmap V.toList . decode @HqdmBinaryRelation NoHeader <$> BL.readFile hqdmRelationsInputFilename
  let relationsInputModel =  csvRelationsToPure $ fromRight [] hqdmRelationSets

  temporalAlgebraTestTriples <- fmap V.toList . decode @HqdmLib.HqdmTriple NoHeader <$> BL.readFile temporalAlgebraTestFilename
  let temporalAlgebraTestModel = fromRight [] temporalAlgebraTestTriples
  let convertedTAStrings = StringUtils.listRemoveDuplicates $ StringUtils.stringTuplesFromTriples temporalAlgebraTestModel []
  let finalTAMap = Map.fromList convertedTAStrings
  let fullyJoinedTAInputModel = StringUtils.joinStringsFromMap temporalAlgebraTestModel finalTAMap

  putStr "\n\nTesting the Temporal Algebra Test Dataset and Functions:\n\n"
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

  -- Now conduct the tests
  hspec simpleStringToUUIDv1Spec
  defaultMain (testGroup "Current set of Allen Interval tests on HQDM States"
    [ testCase "TemporalAlgebra Test Case 1" $
        (TimeUtils.temporalOverlapTest testTAObjectZ testTAObjectB fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.AllenNull,
      testCase "TemporalAlgebra Test Case 2" $
        (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectB fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.PrecedesSnd,
      testCase "TemporalAlgebra Test Case 3" $
        (TimeUtils.temporalOverlapTest testTAObjectB testTAObjectA fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.PrecedesFst,
      testCase "TemporalAlgebra Test Case 4" $
        (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectC fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.MeetsSnd,
      testCase "TemporalAlgebra Test Case 5" $
        (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectA fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.MeetsFst,
      testCase "TemporalAlgebra Test Case 6" $
        (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectB fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.OverlapsSnd,
      testCase "TemporalAlgebra Test Case 7" $
        (TimeUtils.temporalOverlapTest testTAObjectB testTAObjectC fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.OverlapsFst,
      testCase "TemporalAlgebra Test Case 8" $
        (TimeUtils.temporalOverlapTest testTAObjectD testTAObjectC fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.StartsSnd,
      testCase "TemporalAlgebra Test Case 9" $
        (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectD fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.StartsFst,
      testCase "TemporalAlgebra Test Case 10" $
        (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectE fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.DuringSnd,
      testCase "TemporalAlgebra Test Case 11" $
        (TimeUtils.temporalOverlapTest testTAObjectE testTAObjectC fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.DuringFst,
      testCase "TemporalAlgebra Test Case 12" $
        (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectF fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.DuringSnd, -- ????
      testCase "TemporalAlgebra Test Case 13" $
        (TimeUtils.temporalOverlapTest testTAObjectF testTAObjectC fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.DuringFst, -- ????
      testCase "TemporalAlgebra Test Case 14" $
        (TimeUtils.temporalOverlapTest testTAObjectC testTAObjectG fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.EndsSnd,
      testCase "TemporalAlgebra Test Case 15" $
        (TimeUtils.temporalOverlapTest testTAObjectG testTAObjectC fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.EndsFst, -- ????
      testCase "TemporalAlgebra Test Case 16" $
        (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectH fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.EqualExtent,
      testCase "TemporalAlgebra Test Case 17" $
        (TimeUtils.temporalOverlapTest testTAObjectI testTAObjectJ fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.EqualExtent,
      testCase "TemporalAlgebra Test Case 18" $
        (TimeUtils.temporalOverlapTest testTAObjectK testTAObjectK fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.EqualExtent,
      testCase "TemporalAlgebra Test Case 19" $
        (TimeUtils.temporalOverlapTest testTAObjectA testTAObjectA fullyJoinedTAInputModel relationsInputModel) @?= TimeUtils.EqualExtent
    ])

simpleStringToUUIDv1Spec :: Spec
simpleStringToUUIDv1Spec = describe "Tests of a basic TimeUtils function" $ do
  let dateTime =  iso8601ParseM "2021-07-05T14:40:25.4368657Z" :: Maybe UTCTime
  context "A known ISO8601 date-time string of 2021-07-05T14:40:25.4368657Z should convert to a the uuidV1 of ef4dfb91-dd9e-11eb-8000-bb3209de79c0" $
    it "It should match: " $
      TimeUtils.uuidFromUTCTime (fromJust dateTime) `shouldBe` "ef4dfb91-dd9e-11eb-8000-bb3209de79c0"


