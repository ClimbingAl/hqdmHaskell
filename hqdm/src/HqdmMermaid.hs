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

module HqdmMermaid (module HqdmMermaid) where

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

{-
-- Generates a list of nodes and edges from a list of nodes
formMermaidNodeEdgesFromList :: [[String]] -> String -> String
formMermaidNodeEdgesFromList lst relType
    | null lst = 


formMermaidTDFromList :: [[String]] -> String
formMermaidTDFromList lst = 
    mermaidGraphTd ++
-}

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