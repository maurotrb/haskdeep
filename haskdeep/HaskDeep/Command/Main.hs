-- |
-- Module      :  Main
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  MIT
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through the directory structure.
-- Use a list of known hashes to audit a set of files.

module HaskDeep.Command.Main
    (
     main
    )
where

import           Prelude hiding (FilePath)

import           Crypto.Classes (Hash)
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Options.Applicative as OA

import           HaskDeep.Command.Options
import           HaskDeep.Computation
import           HaskDeep.HashSet ()
import qualified HaskDeep.HashSet as HS
import qualified HaskDeep.KnownHash as HK


main :: IO ()
main = OA.execParser optionsPI >>= haskdeep

haskdeep :: Options -> IO ()
haskdeep (Options ExecComputation CompMD5      r k) = execComputation md5hash      "md5"      r k
haskdeep (Options ExecComputation CompSHA1     r k) = execComputation sha1hash     "sha1"     r k
haskdeep (Options ExecComputation CompSHA256   r k) = execComputation sha256hash   "sha265"   r k
haskdeep (Options ExecComputation CompSkein512 r k) = execComputation skein512hash "skein512" r k
haskdeep (Options ExecAudit       CompMD5      r k) = execAudit       md5hash      r k
haskdeep (Options ExecAudit       CompSHA1     r k) = execAudit       sha1hash     r k
haskdeep (Options ExecAudit       CompSHA256   r k) = execAudit       sha256hash   r k
haskdeep (Options ExecAudit       CompSkein512 r k) = execAudit       skein512hash r k

execComputation :: Hash ctx a => ComputationMode a -> String -> FilePath -> FilePath -> IO ()
execComputation cm cmd r k = HK.write k cmd =<< compute r cm

execAudit :: Hash ctx a => ComputationMode a -> FilePath -> FilePath -> IO ()
execAudit cm r k = do hashset1 <- HK.read k
                      hashset2 <- compute r cm
                      let (hashdiff1, hashdiff2) = HS.audit hashset1 hashset2
                      putStrLn "-- Not present in known hashes"
                      mapM_ print (HS.toAscList hashdiff1)
                      putStrLn "-- Present only in root directory"
                      mapM_ print (HS.toAscList hashdiff2)
