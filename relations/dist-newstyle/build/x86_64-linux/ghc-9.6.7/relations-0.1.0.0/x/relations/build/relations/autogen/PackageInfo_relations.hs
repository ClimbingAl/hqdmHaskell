{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_relations (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "relations"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Implementation of semi-formal relations for functional implementation of HQDM"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/ClimbingAl/hqdmHaskell/"
