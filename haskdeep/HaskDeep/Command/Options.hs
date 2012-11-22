-- |
-- Module      :  HaskDeep.Command.Options
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through the directory structure.
-- Use a list of known hashes to audit a set of files.

module HaskDeep.Command.Options
    (
     Options(..)
    ,ExecutionTypeOpt(..)
    ,ComputationModeOpt(..)
    ,optionsPI
    )
where

import           Prelude hiding (FilePath)

import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC
import           Options.Applicative

data Options = Options
    { exectype :: ExecutionTypeOpt
    , compmode :: ComputationModeOpt
    , root     :: FilePath
    , known    :: FilePath }

data ExecutionTypeOpt = ExecComputation
                      | ExecAudit

data ComputationModeOpt = CompMD5
                        | CompSHA1
                        | CompSHA256
                        | CompSkein512

fpReader :: String -> Maybe FilePath
fpReader fp = Just $ FSC.decodeString fp

compReader :: String -> Maybe ComputationModeOpt
compReader "md5"      = Just CompMD5
compReader "sha1"     = Just CompSHA1
compReader "sha256"   = Just CompSHA256
compReader "skein512" = Just CompSkein512
compReader _          = Nothing

optionsP :: Parser Options
optionsP = Options
           <$> flag ExecComputation ExecAudit
                   ( long "audit"
                     & short 'a'
                     & help "Audit" )
            <*> nullOption
                    ( long "computation"
                      & short 'c'
                      & metavar "ALGORITHM"
                      & help "Computation mode: md5, sha1, sha256, skein512"
                      & reader compReader)
            <*> nullOption
                    ( long "root"
                      & short 'r'
                      & metavar "DIRNAME"
                      & help "Root directory"
                      & reader fpReader)
            <*> nullOption
                    ( long "known"
                      & short 'k'
                      & metavar "FILENAME"
                      & help "Known hashes file"
                      & reader fpReader)

optionsPI :: ParserInfo Options
optionsPI = info (optionsP <**> helper)
            ( fullDesc
              & progDesc "Computes hashes and audit a set of files"
              & header "haskdeep - file hashing and audit" )
