-- |
-- Module      :  HaskDeep.Configuration
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
module HaskDeep.Configuration
  ( -- * Configuration
    HaskDeepConfiguration (..),
    defaultHaskDeepConfiguration,
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import System.FilePath (FilePath)
import Prelude hiding (FilePath)

-- | HaskDeep configuration.
data HaskDeepConfiguration = HaskDeepConfiguration
  { -- | Root directory
    rootDirectory :: FilePath,
    -- | Known hashes file
    knownHashes :: FilePath,
    -- | Exclude rule (Regex)
    excludeRegex :: Maybe Text,
    -- | Include file modified from
    includeModFrom :: Maybe UTCTime,
    -- | Include file modified up to
    includeModUpTo :: Maybe UTCTime
  }

-- | HaskDeep default configuration.
defaultHaskDeepConfiguration :: HaskDeepConfiguration
defaultHaskDeepConfiguration =
  HaskDeepConfiguration
    { rootDirectory = ".",
      knownHashes = "known.haskdeep",
      excludeRegex = Nothing,
      includeModFrom = Nothing,
      includeModUpTo = Nothing
    }
