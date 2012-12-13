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
-- Computes hashes traversing recursively through a directory structure.
-- Uses a list of known hashes to audit a set of files.
--
-- Internal module.

module HaskDeep.HashSet
    (
     -- * The @HashSet@ type
     HashSet(..)
    ,setSymbol     -- Text -> HashSet -> HashSet
    ,empty         -- HashSet
    ,insert        -- HashInfo -> HashSet -> HashSet
    ,fromList      -- [HashInfo] -> HashSet
    ,toAscList     -- HashSet -> [HashInfo]
    ,audit         -- HashSet -> HashSet -> (HashSet, HashSet)

     -- * The @HashInfo@ type
    ,HashInfo(..)
    ,toByteString  -- HashInfo -> ByteString
    )
where

import           Data.List (intercalate)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Information about the hashed file.
data HashInfo = HashInfo
    { file :: Text        -- ^ Relative file path
    , size :: Integer     -- ^ File size in byte
    , hash :: ByteString  -- ^ File hash
    } deriving (Eq, Ord)

instance Show HashInfo where
    show (HashInfo f s h) = intercalate "," [ show s
                                            , B8.unpack h
                                            , T.unpack f ]

toByteString :: HashInfo -> ByteString
toByteString (HashInfo f s h) = BS.intercalate commaBS [ B8.pack (show s)
                                                       , h
                                                       , TE.encodeUtf8 f ]

commaBS :: ByteString
commaBS = B8.singleton ','

-- | Hashed files.
data HashSet = HashSet
    { filesCount :: Integer       -- ^ Number of hashed files
    , sizeSum    :: Integer       -- ^ Total size in bytes of hashed files
    , compSymbol :: Text          -- ^ Computation mode symbol
    , hashSet    :: Set HashInfo  -- ^ Hashes
    } deriving (Eq, Ord)

instance Show HashSet where
    show (HashSet 0  _  _  _ ) = "No files."
    show (HashSet fc ss cs hs) = unlines [ "Files count  : " ++ show fc
                                         , "Files size   : " ++ show ss
                                         , "Computed with: " ++ T.unpack cs
                                         , "--"
                                         , (unlines . map show . Set.toAscList) hs ]

-- | Set computation mode symbol.
setSymbol :: Text -> HashSet -> HashSet
setSymbol cs hs = hs { compSymbol = cs }

-- | Create an empty @HashSet@.
empty :: HashSet
empty = HashSet 0 0 T.empty Set.empty

-- | Insert an @HashInfo@ into an @HashSet@
insert :: HashInfo -> HashSet -> HashSet
insert hi hs = hs
               { filesCount = filesCount hs + 1
               , sizeSum    = sizeSum hs + size hi
               , hashSet    = Set.insert hi $ hashSet hs
               }

-- | Create an @HashSet@ from a list of @HashInfo@
fromList :: [HashInfo] -> HashSet
fromList his = HashSet fc ss T.empty hs
    where
      hs = Set.fromList his
      fc = countFiles hs
      ss = sumSize hs

-- | Create an ordered list of @HashInfo@ from an @HashSet@.
toAscList :: HashSet -> [HashInfo]
toAscList (HashSet _ _ _ hs) = Set.toAscList hs

-- | Get the number of hashed files
countFiles :: Set HashInfo -> Integer
countFiles = fromIntegral . Set.size

-- | Get the total size in bytes of hashed files
sumSize :: Set HashInfo -> Integer
sumSize = Set.foldr sumSize' 0
    where
      sumSize' hi ss = ss + size hi

-- | Compare two @HashSet@ and return the not matching @HashInfo@.
--
-- Not matching means all the HashInfo of the first HashSet not present in the second HashSet
-- and all the HashInfo of the second HashSet not present in the first HashSet.
audit :: HashSet -> HashSet -> (HashSet, HashSet)
audit (HashSet _ _ cs1 hs1) (HashSet _ _ cs2 hs2) =
    ( HashSet hs1_no_match_filescount hs1_no_match_sizesum cs1 hs1_no_match
    , HashSet hs2_no_match_filescount hs2_no_match_sizesum cs2 hs2_no_match )
        where
          matching                = hs1 `Set.intersection` hs2
          hs1_no_match            = hs1 `Set.difference` matching
          hs2_no_match            = hs2 `Set.difference` matching
          hs1_no_match_filescount = countFiles hs1_no_match
          hs1_no_match_sizesum    = sumSize hs1_no_match
          hs2_no_match_filescount = countFiles hs2_no_match
          hs2_no_match_sizesum    = sumSize hs2_no_match
