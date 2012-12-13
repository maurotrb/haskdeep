{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  HaskDeep.Computation.Writer
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

module HaskDeep.KnownHash.Writer
    (
     -- * Write the known hashes file
     writeHashes  -- HaskDeepConfiguration -> HashSet -> IO ()
    )
where

import           Prelude hiding (FilePath)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Conduit (($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import           Data.Text ()
import qualified Data.Text.Encoding as TE
import           Filesystem.Path ()
import qualified Filesystem.Path.CurrentOS as FSC

import           HaskDeep.Configuration
import           HaskDeep.HashSet (HashSet)
import qualified HaskDeep.HashSet as HS

-- | Write an @HashSet@ to a known hashes file.
writeHashes :: HaskDeepConfiguration -- ^ Configuration
            -> HashSet               -- ^ @HashSet@ to write
            -> IO ()
writeHashes conf hs = C.runResourceT $ CL.sourceList bs_known $$ CF.sinkFile (knownHashes conf)
    where
      newline       = "\n" :: ByteString
      root          =  TE.encodeUtf8 $ either id id $ FSC.toText $ rootDirectory conf
      ignore        = case ignoreRule conf of
                        (Just rule) -> TE.encodeUtf8 rule
                        Nothing     -> BS.empty
      files_count   = B8.pack $ show $ HS.filesCount hs
      size_sum      = B8.pack $ show $ HS.sizeSum hs
      known_header1 = "%%%% HASHDEEP-1.0"
                      `BS.append` newline
      known_header2 = "%%%% size,"
                      `BS.append` TE.encodeUtf8 (HS.compSymbol hs)
                      `BS.append` ",filename"
                      `BS.append` newline
      bs_comments   = map (flip BS.append newline . BS.append "## ")
                      [ "Root directory: " `BS.append` root
                      , "Ignore rule   : " `BS.append` ignore
                      , "Files count   : " `BS.append` files_count
                      , "Files size    : " `BS.append` size_sum
                      , "" ]
      bs_hash_set   = map (flip BS.append newline . HS.toByteString) $ HS.toAscList hs
      bs_known      = [known_header1, known_header2] ++ bs_comments ++ bs_hash_set
