-- |
-- Module      :  HaskDeep.Command.Main
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through a directory structure.
-- Uses a list of known hashes to audit a set of files.

module Main
    (
     main
    )
where

import           Prelude hiding (FilePath)

import           Crypto.Classes (Hash)
import qualified Options.Applicative as OA

import           HaskDeep
import           Options

main :: IO ()
main = OA.execParser optionsPI >>= haskdeep

haskdeep :: Options -> IO ()
haskdeep (Options OptComputation OptMD5      conf) = execComputation conf md5hash
haskdeep (Options OptComputation OptSHA1     conf) = execComputation conf sha1hash
haskdeep (Options OptComputation OptSHA256   conf) = execComputation conf sha256hash
haskdeep (Options OptComputation OptSkein512 conf) = execComputation conf skein512hash
haskdeep (Options OptAudit       OptMD5      conf) = execAudit       conf md5hash
haskdeep (Options OptAudit       OptSHA1     conf) = execAudit       conf sha1hash
haskdeep (Options OptAudit       OptSHA256   conf) = execAudit       conf sha256hash
haskdeep (Options OptAudit       OptSkein512 conf) = execAudit       conf skein512hash

execComputation :: Hash ctx a => HaskDeepConfiguration -> ComputationMode a -> IO ()
execComputation conf cm = writeHashes conf =<< compute conf cm

execAudit :: Hash ctx a => HaskDeepConfiguration -> ComputationMode a -> IO ()
execAudit conf cm = do hashset1 <- readHashes conf
                       hashset2 <- compute conf cm
                       let (hashdiff1, hashdiff2) = audit hashset1 hashset2
                       putStrLn "**** Not present in known hashes"
                       print hashdiff1
                       putStrLn "**** Present only in root directory"
                       print hashdiff2
