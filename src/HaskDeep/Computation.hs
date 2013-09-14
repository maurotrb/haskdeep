{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
-- |
-- Module      :  HaskDeep.Computation
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through a directory structure.
-- Uses a list of known hashes to audit a set of files.
--
-- Internal module.

module HaskDeep.Computation
    (
     -- * Computation
     compute  -- Hash ctx a => HaskDeepConfiguration -> ComputationMode a -> IO HashSet
    )
where

import           Control.Monad (liftM)
import           Data.Maybe (fromJust)
import           Prelude hiding (FilePath)

import           Crypto.Classes (Hash)
import qualified Crypto.Conduit as CC
import           Data.Conduit (($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Filesystem as FS
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC
import           Text.Regex.TDFA ((=~))
import           Text.Regex.TDFA.Text ()

import           HaskDeep.Configuration
import           HaskDeep.ComputationMode
import           HaskDeep.HashSet (HashInfo(..), HashSet)
import qualified HaskDeep.HashSet as HS

-- | Compute @HashSet@ traversing recursively through the directory structure.
compute :: Hash ctx a => HaskDeepConfiguration
        -> ComputationMode a
        -> IO HashSet
compute conf cm = CF.traverse False root $$ CL.foldM insert_hash empty_with_symbol
    where
      root              = rootDirectory conf
      regex             = excludeRegex conf
      mod_from          = includeModFrom conf
      mod_upto          = includeModUpTo conf
      empty_with_symbol = HS.setSymbol (symbol cm) HS.empty

      insert_hash :: HashSet -> FilePath -> IO HashSet
      insert_hash hs fp = do let fpt = fpToText $ relativize root fp
                             fmt <- FS.getModified fp
                             if (excRegex fpt regex) || (excMod fmt mod_from mod_upto)
                             then return hs
                             else do s <- FS.getSize fp
                                     h <- liftM (runComputation cm) $ C.runResourceT
                                          $ CF.sourceFile fp $$ CC.sinkHash
                                     return $ HS.insert (HashInfo fpt s h) hs

relativize :: FilePath -> FilePath -> FilePath
relativize root fp = fromJust $ FSC.stripPrefix root fp

fpToText :: FilePath -> T.Text
fpToText = either id id . FSC.toText

excRegex :: T.Text -> Maybe T.Text -> Bool
excRegex fpt (Just rule) = fpt =~ rule
excRegex _   Nothing     = False

excMod :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Bool
excMod fmt (Just modFrom) (Just modUpTo) = (fmt < modFrom) || (fmt > modUpTo)
excMod fmt (Just modFrom) Nothing        = fmt < modFrom
excMod fmt Nothing        (Just modUpTo) = fmt > modUpTo
excMod _   Nothing        Nothing        = False