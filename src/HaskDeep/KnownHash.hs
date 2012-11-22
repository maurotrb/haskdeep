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
-- Computes hashes traversing recursively through the directory structure.
-- Use a list of known hashes to audit a set of files.

module HaskDeep.KnownHash
    (
     -- * Read the known hashes file
     read

     -- * Write the known hashes file
    ,write
    )
where

import           Control.Applicative ((<*), many)
import           Control.Monad (liftM)
import           Data.Maybe(fromJust)
import           Data.Word (Word8)
import           Prelude hiding (FilePath, read)

import           Data.Attoparsec (Parser)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.Attoparsec.Combinator as AC
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Conduit (($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import           Filesystem.Path(FilePath)
import qualified Filesystem.Path.CurrentOS as FSC

import           HaskDeep.HashSet (HashSet, HashInfo(..))
import qualified HaskDeep.HashSet as HS

-- | Read known hashes file into an HashSet
read :: FilePath   -- ^ Path of the known hashes file
     -> IO HashSet -- ^ HashSet created from the file
read known = liftM HS.fromList $ C.runResourceT $ CF.sourceFile known $$ CA.sinkParser knownHashes

knownHashes :: Parser [HashInfo]
knownHashes = do _ <- AC.count 2 header
                 _ <- many comment
                 AC.many1 fileinfo

header :: Parser ()
header = do _ <- A8.string "%%%%"
            _ <- A.skipWhile (not . isNewline)
            skipNewline

comment :: Parser ()
comment = do _ <- A8.char8 '#'
             A.skipWhile (not . isNewline)
             skipNewline

fileinfo :: Parser HashInfo
fileinfo = do sizep  <- A.takeTill isComma   <* skipComma
              hashp  <- A.takeTill isComma   <* skipComma
              filep  <- A.takeTill isNewline <* skipNewline
              let filep'      = FSC.decode filep
                  (sizep', _) = fromJust $ B8.readInteger sizep
              return $ HashInfo filep' sizep' hashp

isComma   :: Word8 -> Bool
isComma w = w == 44

skipComma :: Parser ()
skipComma = A.skip isComma

isNewline   :: Word8 -> Bool
isNewline w = w == 10

skipNewline :: Parser ()
skipNewline = A.skip isNewline


-- | Write an HashSet into a known hashes file
write :: FilePath -- ^ Path of the known hashes file
      -> String   -- ^ Computation method description
      -> HashSet  -- ^ HashSet to write to the file
      -> IO ()
write known cmdesc hashset = C.runResourceT $ CL.sourceList bs_known $$ CF.sinkFile known
    where
      known_header1 = "%%%% HASHDEEP-1.0\n" :: ByteString
      known_header2 = "%%%% size," `BS.append` B8.pack cmdesc `BS.append` ",file\n" :: ByteString
      newline = "\n" :: ByteString
      bs_hash_set = map (flip BS.append newline . B8.pack . show) $ HS.toAscList hashset
      bs_known    = [known_header1, known_header2] ++ bs_hash_set
