--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  HqdmMermaid
-- Description :  Module with functions to export hqdm data as Mermaid graphs
-- Copyright   :  (c) CIS Ltd
-- License     :  Apache-2.0
--
-- Maintainer  :  aristotlestarteditall@gmail.com
-- Stability   :  experimental
-- Portability :  portable (albeit for HQDM All As Data applications)
--
--

module HqdmMermaid (
  mermaidEntitySupertypeTree,
  mermaidSuperRelationPathsToUniversalRelation,
  mermaidTDTopAndTail,
  mermaidAddTitle,
  insertEntityNodeName,
  insertBRNodeName,
  mermaidSubRelationPathsWithLayerCount,
  mermaidEulerCentralClassDef,
  mermaidEntityEulerTree,
  mermaidAddEulerTitle
) where

import HqdmLib
import HqdmRelations
import Data.List.Split
import HqdmIds (thing)
import Data.String (IsString)

mermaidMkdnStart :: String
mermaidMkdnStart = "``` mermaid\n"

mermaidMkdnEnd :: String
mermaidMkdnEnd = "```\n"

mermaidGraphTd :: String
mermaidGraphTd = "graph TD\n"

mermaidGraphLr :: String
mermaidGraphLr = "graph LR\n"

mermaidStyleStart :: String
mermaidStyleStart = "  style "

mermaidStrokeStyleEnd :: String
mermaidStrokeStyleEnd = " stroke-width:4px;\n"

mermaidArrow :: String
mermaidArrow = "-->"

mermaidArrowNamed :: String
mermaidArrowNamed = "-->|"

mermaidArrowNamedEnd :: String
mermaidArrowNamedEnd = "|"

mermaidArrowDashed :: String
mermaidArrowDashed = "--->"

mermaidCircleStart :: String
mermaidCircleStart = "(("

mermaidCircleEnd :: String
mermaidCircleEnd = "))"

mermaidStadiumStart :: String
mermaidStadiumStart = "(["

mermaidStadiumEnd :: String
mermaidStadiumEnd = "])"

mermaidRoundStart :: String
mermaidRoundStart = "("

mermaidRoundEnd :: String
mermaidRoundEnd = ")"

mermaidSuperRelOf :: String
mermaidSuperRelOf = "-->|superRelationOf|"

lineEnd :: String
lineEnd = ";\n"

mermaidNodePaddingClassName :: String
mermaidNodePaddingClassName = ":::stdSize"

mermaidEulerCentralClassDef :: String
mermaidEulerCentralClassDef = "classDef specialSize fill:#c8e4fa,stroke:#4698eb,stroke-width:2px,color:#000;\n"

mermaidEulerCentralClassName :: String
mermaidEulerCentralClassName = ":::specialSize;"

mermaidNodePaddingClassDef :: String
mermaidNodePaddingClassDef = "classDef stdSize padding:125px,stroke-width:6px,font-size:20pt,stroke:#000\nclassDef specialSize padding:125px,stroke-width:6px,font-size:20pt,stroke:#000, fill:#e38952\nclassDef foundationSize padding:125px,stroke-width:6px,font-size:20pt,stroke:#000, fill:#52a4e3\n"

mermaidAddTitle :: String -> String -> String
mermaidAddTitle mm title = "--- \ntitle: " ++ title ++ "\nconfig:\n  layout: neutral\n  look: handDrawn\n---\n" ++ mm

mermaidAddEulerTitle :: String -> String -> String
mermaidAddEulerTitle mm title = "--- \ntitle: " ++ title ++ "\nconfig:\n  theme: default\n  look: classic\n---\n" ++ mm

mermaidTDTopAndTail :: String -> String
mermaidTDTopAndTail body = "%%{init: { \"flowchart\": { \"htmlLabels\": true, \"curve\": \"linear\" } } }%%\ngraph TD\n" ++ mermaidNodePaddingClassDef ++ body ++ "\n"

insertBRsinString :: String -> String
insertBRsinString str = concatMap (++ " <br> ") (splitOn "_" str)

insertEntityNodeName :: HqdmLib.Id -> [HqdmLib.HqdmTriple] -> String
insertEntityNodeName id hqdm = "\t" ++ id ++ "[" ++ insertBRsinString ( HqdmLib.headIfStringPresent (HqdmLib.lookupHqdmTypeFromAll hqdm id)) ++ "]" ++ mermaidNodePaddingClassName ++ ";\n"

insertBRNodeName :: HqdmRelations.RelationId -> [HqdmRelations.HqdmBinaryRelationPure] -> String
insertBRNodeName id brels = "\t" ++ id ++ "[" ++ id ++ " <BR> " ++ HqdmRelations.getPureRelationName (head $ HqdmRelations.findBrelFromId id brels) ++ "]" ++ mermaidNodePaddingClassName ++ ";\n"

-- | mermaidEntitySupertypeTree
-- From all the triples given by lookupSupertypes find all the supertypes of a given node Id
-- (supplied as a [[id]]). This takes hqdm type triples as [HqdmTriple].
-- The ouput is a list of mermaid nodes and connections between them.
mermaidEntitySupertypeTree :: [[HqdmLib.Id]] -> [HqdmLib.HqdmTriple] -> String -> String
mermaidEntitySupertypeTree ids hqdm mmNodes= go ids hqdm mmNodes
  where
    nextLayer = last ids
    possibleNewLayer = HqdmLib.uniqueIds $ concat (HqdmLib.lookupSupertypesOf nextLayer hqdm)
    newLayer = [HqdmLib.deleteItemsFromList possibleNewLayer (take 1 nextLayer)]
    nextMmNodes = concat $ concatMap (\ x ->
        fmap (\ y ->
            "\t" ++ y ++ "[" ++ insertBRsinString ( HqdmLib.headIfStringPresent (HqdmLib.lookupHqdmTypeFromAll hqdm y)) ++ "]" ++ mermaidNodePaddingClassName ++ ";\n"
                ++ "\t" ++ y ++ "-->|supertype_of|" ++ x ++ ";\n"
            ) (HqdmLib.lookupSupertypeOf x hqdm)) nextLayer

    go ids hqdm mmNodes
      | null newLayer = mmNodes
      | newLayer == [[]] = mmNodes
      | sum [length $ filter (== thing) yl | yl <- newLayer] > 0 = nextMmNodes ++ mmNodes
      | otherwise = mermaidEntitySupertypeTree (ids ++ newLayer) hqdm ( nextMmNodes ++ mmNodes )

-- | mermaidEntityEulerTree
-- From all the triples given by lookupSupertypes find all the supertypes of a given node Id
-- (supplied as a [[id]]). This takes hqdm type triples as [HqdmTriple].
-- The ouput is a list of mermaid nodes and connections between them.
mermaidEntityEulerTree :: [[HqdmLib.Id]] -> [HqdmLib.HqdmTriple] -> String -> String
mermaidEntityEulerTree ids hqdm mmNodes= go ids hqdm mmNodes
  where
    nextLayer = last ids
    possibleNewLayer = HqdmLib.uniqueIds $ concat (HqdmLib.lookupSupertypesOf nextLayer hqdm)
    newLayer = [HqdmLib.deleteItemsFromList possibleNewLayer (take 1 nextLayer)]
    nextMmNodes = concatMap (\ x -> HqdmLib.headIfStringPresent (HqdmLib.lookupHqdmTypeFromAll hqdm x) ++ " ") (concat newLayer)
    mmLayer =  "subgraph " ++ concat (concat newLayer) ++ "[\"" ++ nextMmNodes ++ "\"];\n" ++ mmNodes ++ "\tend\n"

    go ids hqdm mmNodes
      | null newLayer = mmNodes
      | newLayer == [[]] = mmNodes
      | sum [length $ filter (== thing) yl | yl <- newLayer] > 0 = mmLayer
      | otherwise = mermaidEntityEulerTree (ids ++ newLayer) hqdm mmLayer

-- | mermaidSuperRelationPathsToUniversalRelation
-- From all the Binary Relations given find all the BR supertypes of a given RelationId
-- (supplied as a [[RelationId]]). This takes only has_supertype statements as [HqdmTriple].
-- The ouput is a list of mermaid nodes and connections between them.
-- Scratch: HqdmRelations.getPureRelationId $ head (HqdmRelations.findBrelFromId y brels)
mermaidSuperRelationPathsToUniversalRelation :: [[RelationId]] -> [HqdmBinaryRelationPure] -> String -> String
mermaidSuperRelationPathsToUniversalRelation relIds brels mmNodes = go relIds brels mmNodes
  where
    nextLayer = last relIds
    superBRs = HqdmRelations.getPureSuperRelations $ HqdmRelations.findBrelsFromIds nextLayer brels
    newLayer = [ HqdmLib.uniqueIds $ HqdmLib.deleteItemsFromList superBRs nextLayer]
    nextMmNodes = concat $ concatMap (\ x ->
        fmap (\ y ->
            "\t" ++ y ++ "[" ++ y ++ " <BR> " ++ HqdmRelations.getPureRelationName (head $ HqdmRelations.findBrelFromId y brels) ++ "]" ++ mermaidNodePaddingClassName ++ ";\n"
                ++ "\t" ++ y ++ "-->|superBinaryRel_of|" ++ x ++ ";\n"
            ) (HqdmRelations.getPureSuperRelation (head $ HqdmRelations.findBrelFromId x brels))) nextLayer
    -- newLayer is formed from a defence against circularity.  Remove elements of newLayer that are in nextLayer.

    go relIds brels mmNodes
      | null newLayer = init mmNodes
      | newLayer == [[]] = mmNodes
      | sum [length $ filter (== HqdmRelations.universalRelationSet) yl | yl <- newLayer] > 0 = nextMmNodes ++ mmNodes
      | otherwise = mermaidSuperRelationPathsToUniversalRelation (relIds ++ newLayer) brels ( nextMmNodes ++ mmNodes )

-- | mermaidSubRelationPathsWithLayerCount
-- From all the Binary Relations given find all the BR supertypes of a given RelationId
-- (supplied as a [[RelationId]]). This takes only has_supertype statements as [HqdmTriple].
-- The ouput is a list of mermaid nodes and connections between them.
-- Scratch: HqdmRelations.getPureRelationId $ head (HqdmRelations.findBrelFromId y brels)
mermaidSubRelationPathsWithLayerCount :: [[RelationId]] -> [HqdmBinaryRelationPure] -> Int -> String -> String
mermaidSubRelationPathsWithLayerCount relIds brels cnt mmNodes = go relIds brels cnt mmNodes
  where
    nextLayer = last relIds
    subBRs = concat $ HqdmRelations.lookupSubBRelsOf nextLayer brels
    newLayer = [ HqdmLib.uniqueIds $ HqdmLib.deleteItemsFromList subBRs nextLayer]
    nextMmNodes = concat $ concatMap (\ x ->
        fmap (\ y ->
            "\t" ++ x ++ "[" ++ x ++ " <BR> " ++ HqdmRelations.getPureRelationName (head $ HqdmRelations.findBrelFromId x brels) ++ "]" ++ mermaidNodePaddingClassName ++ ";\n"
                ++ "\t" ++ y ++ "-->|superBinaryRel_of|" ++ x ++ ";\n"
            ) (HqdmRelations.getPureSuperRelation (head $ HqdmRelations.findBrelFromId x brels))) (concat newLayer)
    -- newLayer is formed from a defence against circularity.  Remove elements of newLayer that are in nextLayer.

    go relIds brels cnt mmNodes
      | cnt == 0 = mmNodes
      | null newLayer = init mmNodes
      | newLayer == [[]] = mmNodes
      | otherwise = mermaidSubRelationPathsWithLayerCount (relIds ++ newLayer) brels (subtract 1 cnt) ( nextMmNodes ++ mmNodes )

{-/*
    * EXAMPLE TYPE HIERARCHY
    * 
    * ``` mermaid
    * graph TD
    * id1[thing];
    * style id1 stroke-width:4px;
    * id2[abstract_object] & id3[spatio_temporal <br> _extent];
    * id1-->|supertype_of|id2;
    * id1-->|supertype_of|id3;
    * id4[class];
    * id2-->|supertype_of|id4;
    * id1-...->|member__of|id4;
    * ```
    * 
    */-}