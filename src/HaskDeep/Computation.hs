{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}

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
  ( -- * Computation
    compute, -- Hash ctx a => HaskDeepConfiguration -> ComputationMode a -> IO HashSet
  )
where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Crypto.Classes (Hash)
import qualified Crypto.Conduit as CC
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CCB
import qualified Data.Text as T
import Data.Time (UTCTime)
import HaskDeep.ComputationMode
import HaskDeep.Configuration
import HaskDeep.HashSet (HashInfo (..), HashSet)
import qualified HaskDeep.HashSet as HS
import qualified System.Directory as D
import System.FilePath (FilePath, makeRelative)
import qualified System.PosixCompat.Files as PF
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import Prelude hiding (FilePath)

-- | Compute @HashSet@ traversing recursively through the directory structure.
compute ::
  Hash ctx a =>
  HaskDeepConfiguration ->
  ComputationMode a ->
  IO HashSet
compute conf cm = runResourceT $ runConduit $ CCB.sourceDirectoryDeep False root .| CCB.foldM insert_hash empty_with_symbol
  where
    root = rootDirectory conf
    regex = excludeRegex conf
    mod_from = includeModFrom conf
    mod_upto = includeModUpTo conf
    empty_with_symbol = HS.setSymbol (symbol cm) HS.empty

    insert_hash :: HashSet -> FilePath -> ResourceT IO HashSet
    insert_hash hs fp = do
      let fpt = T.pack $ makeRelative root fp
      fmt <- liftIO $ D.getModificationTime fp
      if excRegex fpt regex || excMod fmt mod_from mod_upto
        then return hs
        else do
          s <- liftIO $ getFileSize fp
          h <-
            liftM (runComputation cm) $
              runResourceT $
                runConduit $ CB.sourceFile fp .| CC.sinkHash
          return $ HS.insert (HashInfo fpt s h) hs

getFileSize :: FilePath -> IO Integer
getFileSize path = do
  stat <- PF.getFileStatus path
  return (toInteger $ PF.fileSize stat)

excRegex :: T.Text -> Maybe T.Text -> Bool
excRegex fpt (Just rule) = fpt =~ rule
excRegex _ Nothing = False

excMod :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Bool
excMod fmt (Just modFrom) (Just modUpTo) = (fmt < modFrom) || (fmt > modUpTo)
excMod fmt (Just modFrom) Nothing = fmt < modFrom
excMod fmt Nothing (Just modUpTo) = fmt > modUpTo
excMod _ Nothing Nothing = False
