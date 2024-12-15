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
  insertNodeDefinition
) where

import HqdmLib 
import HqdmRelations
import Data.List.Split
import HqdmIds (thing)

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

mermaidNodePaddingClassDef :: String
mermaidNodePaddingClassDef = "\tclassDef stdSize padding:125px,stroke-width:6px,font-size:20pt,stroke:#000\n"

mermaidAddTitle :: String -> String -> String
mermaidAddTitle mm title = "--- \ntitle: " ++ title ++ "\n---\n" ++ mm

mermaidTDTopAndTail :: String -> String
mermaidTDTopAndTail body = "%%{init: { \"flowchart\": { \"htmlLabels\": true, \"curve\": \"linear\" } } }%%\ngraph TD\n" ++ mermaidNodePaddingClassDef ++ body ++ "\n"

insertBRsinString :: String -> String
insertBRsinString str = concatMap (++ " <br> ") (splitOn "_" str)

insertNodeDefinition :: HqdmLib.Id -> [HqdmLib.HqdmTriple] -> String
insertNodeDefinition id hqdm = "\t" ++ id ++ "[" ++ insertBRsinString ( HqdmLib.headIfStringPresent (HqdmLib.lookupHqdmTypeFromAll hqdm id)) ++ "]" ++ mermaidNodePaddingClassName ++ ";\n"

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
    -- newLayer is formed from a defence against circularity.  Remove elements of newLayer that are in nextLayer.

    go ids hqdm mmNodes
      | null newLayer = mmNodes
      | newLayer == [[]] = mmNodes
      | sum [length $ filter (== thing) yl | yl <- newLayer] > 0 = nextMmNodes ++ mmNodes
      | otherwise = mermaidEntitySupertypeTree (ids ++ newLayer) hqdm ( nextMmNodes ++ mmNodes )

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
            "\t" ++ y ++ "[" ++ insertBRsinString y ++ "]" ++ mermaidNodePaddingClassName ++ ";\n" 
                ++ "\t" ++ y ++ "-->|superBinaryRel_of|" ++ x ++ ";\n"
            ) (HqdmRelations.getPureSuperRelation (head $ HqdmRelations.findBrelFromId x brels))) nextLayer
    -- newLayer is formed from a defence against circularity.  Remove elements of newLayer that are in nextLayer.

    go relIds brels mmNodes
      | null newLayer = init mmNodes
      | newLayer == [[]] = mmNodes
      | sum [length $ filter (== HqdmRelations.universalRelationSet) yl | yl <- newLayer] > 0 = nextMmNodes ++ mmNodes 
      | otherwise = mermaidSuperRelationPathsToUniversalRelation (relIds ++ newLayer) brels ( nextMmNodes ++ mmNodes )

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