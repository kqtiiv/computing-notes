{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hft (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hft"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Haskell Final Test 24/25"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
