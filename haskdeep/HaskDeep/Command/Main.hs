{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  HaskDeep.Command.Main
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
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

import           Control.Monad (liftM)
import           Prelude hiding (FilePath)

import           Crypto.Classes (Hash)
import           Data.Text ()
import qualified Data.Text as T
import           Data.Time.Clock ()
import qualified Data.Time.Clock as TC
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC
import qualified Options.Applicative as OA

import           HaskDeep.Command.Options
import           HaskDeep.Computation
import           HaskDeep.HashSet ()
import qualified HaskDeep.HashSet as HS
import qualified HaskDeep.KnownHash as HK

main :: IO ()
main = OA.execParser optionsPI >>= haskdeep

haskdeep :: Options -> IO ()
haskdeep (Options OptComputation OptMD5      ir ik) = execComputation md5hash      ir ik
haskdeep (Options OptComputation OptSHA1     ir ik) = execComputation sha1hash     ir ik
haskdeep (Options OptComputation OptSHA256   ir ik) = execComputation sha256hash   ir ik
haskdeep (Options OptComputation OptSkein512 ir ik) = execComputation skein512hash ir ik
haskdeep (Options OptAudit       OptMD5      ir ik) = execAudit       md5hash      ir ik
haskdeep (Options OptAudit       OptSHA1     ir ik) = execAudit       sha1hash     ir ik
haskdeep (Options OptAudit       OptSHA256   ir ik) = execAudit       sha256hash   ir ik
haskdeep (Options OptAudit       OptSkein512 ir ik) = execAudit       skein512hash ir ik

execComputation :: Hash ctx a => ComputationMode a -> FilePath -> FilePath -> IO ()
execComputation cm r k = do start   <- liftM show TC.getCurrentTime
                            hashset <- compute r cm
                            end     <- liftM show TC.getCurrentTime
                            let comments = [ "Root directory: " `T.append` either id id (FSC.toText r)
                                           , "Started at    : " `T.append` T.pack start
                                           , "Ended at      : " `T.append` T.pack end
                                           , "Files count   : " `T.append` T.pack (show $ HS.filesCount hashset)
                                           , "Files size    : " `T.append` T.pack (show $ HS.sizeSum hashset)
                                           ]
                            HK.write k hashset comments

execAudit :: Hash ctx a => ComputationMode a -> FilePath -> FilePath -> IO ()
execAudit cm r k = do hashset1 <- HK.read k
                      hashset2 <- compute r cm
                      let (hashdiff1, hashdiff2) = HS.audit hashset1 hashset2
                      putStrLn "-- Not present in known hashes"
                      mapM_ print (HS.toAscList hashdiff1)
                      putStrLn "-- Present only in root directory"
                      mapM_ print (HS.toAscList hashdiff2)
