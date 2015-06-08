{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Stats
-- Copyright   :  (c) Taru Karttunen 2015
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Write out Hoogle compatible documentation
-- http://www.haskell.org/hoogle/
-----------------------------------------------------------------------------
module Haddock.Backends.Stats (
    ppStats
  ) where

import Haddock.Types hiding (Version)
import GHC

import Data.List(foldl')
import qualified Data.Map as Map
import System.FilePath

ppStats :: String -> Maybe (Doc RdrName) -> [Interface] -> FilePath -> IO ()
ppStats _synopsis _prologue ifaces odir = do
    let filename = "stats.json"
        out = "{\n\"sum-ndoc\":"++show (csNDoc totals)++",\n"++
              "\"sum-warn\":"++show (csNWarn totals)++",\n"++
              "\"sum-ntot\":"++show (csNTot totals)++"\n}\n"
        totals = foldl' statsUpdate (CollectStats 0 0 0) ifaces
    writeFile (odir </> filename) out

data CollectStats = CollectStats { csNTot :: !Int, csNDoc :: !Int, csNWarn :: !Int }

statsUpdate :: CollectStats -> Interface -> CollectStats
statsUpdate (CollectStats c d e) iface = CollectStats (a+c) (b+d) (e+nw)
  where (a,b) = ifaceHaddockCoverage iface
        nw = Map.size $ ifaceWarningMap iface
