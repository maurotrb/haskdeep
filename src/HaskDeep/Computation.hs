-- |
-- Module      :  HaskDeep.Computation
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through the directory structure.
-- Use a list of known hashes to audit a set of files.

module HaskDeep.Computation
    (
    -- * Computation
     compute  -- Hash ctx a => FilePath -> ComputationMode a -> IO HashSet

    -- * Computation modes: MD5, SHA1 ...
    ,ComputationMode  -- a -> Bytestring
    ,md5hash
    ,sha1hash
    ,sha256hash
    ,skein512hash
    )
where

import           Control.Monad (liftM)
import           Data.Bits ((.&.), shiftR)
import           Data.Maybe (fromJust)
import           Data.Word ()
import           Prelude hiding (FilePath)

import           Crypto.Classes (Hash)
import qualified Crypto.Conduit as CC
import           Crypto.Hash.MD5 (MD5)
import           Crypto.Hash.SHA1 (SHA1)
import           Crypto.Hash.SHA256 (SHA256)
import           Crypto.Hash.Skein512 (Skein512)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Conduit (($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import qualified Data.Serialize as S
import qualified Filesystem as FS
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC

import           HaskDeep.HashSet (HashInfo(..), HashSet)
import qualified HaskDeep.HashSet as HS

-- | Algorithm to compute hash
type ComputationMode a = a -> ByteString

md5hash  :: ComputationMode MD5
md5hash  = toHex . S.encode

sha1hash :: ComputationMode SHA1
sha1hash = toHex . S.encode

sha256hash :: ComputationMode SHA256
sha256hash = toHex . S.encode

skein512hash :: ComputationMode Skein512
skein512hash = toHex . S.encode

-- | Compute @HashSet@ traversing recursively through the directory structure.
compute :: Hash ctx a => FilePath -- ^ Root path
        -> ComputationMode a      -- ^ Algorithm to compute hash
        -> IO HashSet             -- ^ Computed HashSet
compute root cm = CF.traverse False root $$ CL.foldM insert_hash HS.empty
    where
      insert_hash :: HashSet -> FilePath -> IO HashSet
      insert_hash hs fp = do
        s     <- FS.getSize fp
        h     <- liftM cm $ C.runResourceT $ CF.sourceFile fp $$ CC.sinkHash
        let p = either id id $ FSC.toText $ fromJust $ FSC.stripPrefix root fp
        return $ HS.insert (HashInfo p s h) hs

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