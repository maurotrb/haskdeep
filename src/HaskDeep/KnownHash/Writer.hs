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
  ( -- * Write the known hashes file
    writeHashes, -- HaskDeepConfiguration -> HashSet -> IO ()
  )
where

import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as DT
import HaskDeep.Configuration
import HaskDeep.HashSet (HashSet)
import qualified HaskDeep.HashSet as HS
import System.FilePath ()
import Prelude hiding (FilePath)

-- | Write an @HashSet@ to a known hashes file.
writeHashes ::
  -- | Configuration
  HaskDeepConfiguration ->
  -- | @HashSet@ to write
  HashSet ->
  IO ()
writeHashes conf hs = runResourceT $ runConduit $ CL.sourceList bs_known .| CB.sinkFile (knownHashes conf)
  where
    newline = "\n" :: ByteString
    root = TE.encodeUtf8 $ T.pack $ rootDirectory conf
    exclude_regex = case excludeRegex conf of
      (Just regex) -> TE.encodeUtf8 regex
      Nothing -> BS.empty
    include_mod_from = case includeModFrom conf of
      (Just modFrom) -> B8.pack $ DT.formatTime DT.defaultTimeLocale "%FT%TZ" modFrom
      Nothing -> BS.empty
    include_mod_upto = case includeModUpTo conf of
      (Just modUpTo) -> B8.pack $ DT.formatTime DT.defaultTimeLocale "%FT%TZ" modUpTo
      Nothing -> BS.empty
    files_count = B8.pack $ show $ HS.filesCount hs
    size_sum = B8.pack $ show $ HS.sizeSum hs
    known_header1 =
      "%%%% HASHDEEP-1.0"
        `BS.append` newline
    known_header2 =
      "%%%% size,"
        `BS.append` TE.encodeUtf8 (HS.compSymbol hs)
        `BS.append` ",filename"
        `BS.append` newline
    bs_comments =
      map
        (flip BS.append newline . BS.append "## ")
        [ "Root directory    : " `BS.append` root,
          "Exclude regex     : " `BS.append` exclude_regex,
          "Include mod. from : " `BS.append` include_mod_from,
          "Include mod. upto : " `BS.append` include_mod_upto,
          "Files count       : " `BS.append` files_count,
          "Files size        : " `BS.append` size_sum,
          ""
        ]
    bs_hash_set = map (flip BS.append newline . HS.toByteString) $ HS.toAscList hs
    bs_known = [known_header1, known_header2] ++ bs_comments ++ bs_hash_set
