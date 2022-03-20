{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  HaskDeep.Computation.Reader
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

module HaskDeep.KnownHash.Reader
    (
     -- * Read the known hashes file
     readHashes  -- HaskDeepConfiguration -> IO HashSet
    )
where

import           Control.Applicative ((<*), many)
import           Control.Monad (liftM)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Maybe(fromJust)
import           Data.Word (Word8)
import           Prelude hiding (FilePath)

import           Data.Attoparsec (Parser)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.ByteString.Char8 as B8
import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import           Data.Text ()
import qualified Data.Text.Encoding as TE
import           System.FilePath ()

import           HaskDeep.Configuration
import           HaskDeep.HashSet (HashSet, HashInfo(..))
import qualified HaskDeep.HashSet as HS

-- | Read known hashes file into an @HashSet@.
readHashes :: HaskDeepConfiguration -- ^ Configuration
           -> IO HashSet            -- ^ @HashSet@ red from file
readHashes conf = liftM HS.fromList $ runResourceT
                  $ runConduit $ CB.sourceFile (knownHashes conf) .| CA.sinkParser knownHashesP


-- Parsers

knownHashesP :: Parser [HashInfo]
knownHashesP = do _ <- AC.count 2 headerP
                  _ <- many commentP
                  AC.many1 fileinfoP

headerP :: Parser ()
headerP = do _ <- A8.string "%%%%"
             _ <- A.skipWhile (not . isNewline)
             skipNewline

commentP :: Parser ()
commentP = do _ <- A8.char8 '#'
              A.skipWhile (not . isNewline)
              skipNewline

fileinfoP :: Parser HashInfo
fileinfoP = do sizep  <- A.takeTill isComma   <* skipComma
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
