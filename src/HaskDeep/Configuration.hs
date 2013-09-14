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
    (
     -- * Configuration
     HaskDeepConfiguration (..)
    ,defaultHaskDeepConfiguration
    )
where

import           Prelude hiding (FilePath)

import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC

-- | HaskDeep configuration.
data HaskDeepConfiguration = HaskDeepConfiguration
    { rootDirectory   :: FilePath      -- ^ Root directory
    , knownHashes     :: FilePath      -- ^ Known hashes file
    , excludeRegex    :: Maybe Text    -- ^ Exclude rule (Regex)
    , includeModFrom  :: Maybe UTCTime -- ^ Include file modified from
    , includeModUpTo  :: Maybe UTCTime -- ^ Include file modified up to
    }

-- | HaskDeep default configuration.
defaultHaskDeepConfiguration :: HaskDeepConfiguration
defaultHaskDeepConfiguration = HaskDeepConfiguration
    { rootDirectory   = FSC.decodeString "."
    , knownHashes     = FSC.decodeString "known.haskdeep"
    , excludeRegex    = Nothing
    , includeModFrom  = Nothing
    , includeModUpTo  = Nothing
    }
