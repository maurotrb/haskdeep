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
     read   -- FilePath -> IO HashSet

     -- * Write the known hashes file
    ,write  -- HashSet
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
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Filesystem.Path(FilePath)

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
              let filep'      = TE.decodeUtf8 filep
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
      -> HashSet  -- ^ HashSet to write to the file
      -> [Text]   -- ^ Comments
      -> IO ()
write kfp hs cmts = C.runResourceT $ CL.sourceList bs_known $$ CF.sinkFile kfp
    where
      newline       = "\n" :: ByteString
      known_header1 = "%%%% HASHDEEP-1.0"
                      `BS.append` newline
      known_header2 = "%%%% size,"
                      `BS.append` TE.encodeUtf8 (HS.compSymbol hs)
                      `BS.append` ",filename"
                      `BS.append` newline
      bs_comments   = map (flip BS.append newline . BS.append "## " . TE.encodeUtf8) cmts
                      ++ ["## " `BS.append` newline]
      bs_hash_set   = map (flip BS.append newline . HS.toByteString) $ HS.toAscList hs
      bs_known      = [known_header1, known_header2] ++ bs_comments ++ bs_hash_set
