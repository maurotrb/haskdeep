-- |
-- Module      :  HaskDeep
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through a directory structure.
-- Uses a list of known hashes to audit a set of files.
module HaskDeep
  ( -- * The @HashSet@ type
    HashSet (),
    audit,

    -- * Configuration
    HaskDeepConfiguration (..),
    defaultHaskDeepConfiguration,

    -- * Computation modes
    ComputationMode (..),
    md5hash,
    sha1hash,
    sha256hash,
    skein512hash,

    -- * Computation
    compute,

    -- * Known hashes file
    readHashes,
    writeHashes,
  )
where

import HaskDeep.Computation
import HaskDeep.ComputationMode
import HaskDeep.Configuration
import HaskDeep.HashSet
import HaskDeep.KnownHash.Reader
import HaskDeep.KnownHash.Writer
