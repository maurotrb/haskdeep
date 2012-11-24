{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  HaskDeep.HashSet
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through the directory structure.
-- Use a list of known hashes to audit a set of files.

module HaskDeep.HashSet
    (
    -- * The @HashSet@ type
     HashSet()
    ,empty     -- HashSet
    ,insert    -- HashInfo -> HashSet -> HashSet
    ,fromList  -- [HashInfo] -> HashSet
    ,toAscList -- HashSet -> [HashInfo]
    ,audit     -- HashSet -> HashSet -> (HashSet, HashSet)

    -- * The @HashInfo@ type
    ,HashInfo(..)
    ,toByteString -- HashInfo -> ByteString
    )
where

import           Prelude hiding (FilePath)

import           Crypto.Classes ()
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC

-- | Information about the hashed file
data HashInfo = HashInfo
    { file :: FilePath   -- ^ File path, relative
    , size :: Integer    -- ^ File size in byte
    , hash :: ByteString -- ^ File hash
    } deriving (Eq, Ord)

instance Show HashInfo where
    show (HashInfo f s h) = show s
                            ++ ","
                            ++ B8.unpack h
                            ++ ","
                            ++ (T.unpack $ either id id $ FSC.toText f)

toByteString :: HashInfo -> ByteString
toByteString (HashInfo f s h) = (B8.pack $ show s)
                                `BS.append` commaBS
                                `BS.append` h
                                `BS.append` commaBS
                                `BS.append` (TE.encodeUtf8 $ either id id $ FSC.toText f)

commaBS :: ByteString
commaBS = B8.singleton ','

-- | Hashed files
newtype HashSet = HashSet (Set HashInfo)
    deriving (Eq, Ord, Show)

empty :: HashSet
empty = HashSet Set.empty

insert :: HashInfo -> HashSet -> HashSet
insert h (HashSet s) = HashSet $ Set.insert h s

fromList :: [HashInfo] -> HashSet
fromList hs = HashSet $ Set.fromList hs

toAscList :: HashSet -> [HashInfo]
toAscList (HashSet s) = Set.toAscList s

-- | Compare two @HashSet@ and return the not matching @HashInfo@
--
-- Not matching means all the HashInfo of the first HashSet not present in the second HashSet
-- and all the HashInfo of the second HashSet not present in the first HashSet
audit :: HashSet -> HashSet -> (HashSet, HashSet)
audit (HashSet s1) (HashSet s2) = (HashSet s1_not_matching, HashSet s2_not_matching)
    where
      matching        = s1 `Set.intersection` s2
      s1_not_matching = s1 `Set.difference` matching
      s2_not_matching = s2 `Set.difference` matching
