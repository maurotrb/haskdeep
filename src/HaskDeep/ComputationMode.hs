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

module HaskDeep.ComputationMode
    (
     -- * Computation modes
     ComputationMode (..)
    ,md5hash
    ,sha1hash
    ,sha256hash
    ,skein512hash
    )
where

import           Data.Bits ((.&.), shiftR)

import           Crypto.Hash.MD5 (MD5)
import           Crypto.Hash.SHA1 (SHA1)
import           Crypto.Hash.SHA256 (SHA256)
import           Crypto.Hash.Skein512 (Skein512)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text as T

-- | Algorithm to compute hash.
data ComputationMode a = ComputationMode
    { symbol         :: T.Text           -- ^ Computation mode symbol
    , runComputation :: a -> ByteString  -- ^ Computation function
    }

-- | MD5 computation.
md5hash  :: ComputationMode MD5
md5hash  = ComputationMode "md5" (toHex . S.encode)

-- | SHA1 computation.
sha1hash :: ComputationMode SHA1
sha1hash = ComputationMode "sha1" (toHex . S.encode)

-- | SHA256 computation.
sha256hash :: ComputationMode SHA256
sha256hash = ComputationMode "sha256" (toHex . S.encode)

-- | Skein512 computation.
skein512hash :: ComputationMode Skein512
skein512hash = ComputationMode "skein512" (toHex . S.encode)

-- Only with ByteString 0.10
--toHex :: ByteString -> ByteString
--toHex = BS.concat . BL.toChunks . BB.toLazyByteString . BA.byteStringHexFixed

-- Taken from
-- http://stackoverflow.com/questions/10099921/efficiently-turn-a-bytestring-into-a-hex-representation
toHex :: ByteString -> ByteString
toHex bs0 =
    fst $ BS.unfoldrN (BS.length bs0 * 2) go (Left bs0)
  where
    go (Left bs) =
        case BS.uncons bs of
            Nothing -> Nothing
            Just (w, bs') ->
                let w1 = w `shiftR` 4
                    w2 = w .&. 15
                    c1 = toC w1
                    c2 = toC w2
                 in Just (c1, Right (c2, bs'))
    go (Right (c, bs)) = Just (c, Left bs)

    toC w
        | w < 10 = w + 48
        | otherwise = w + 87
