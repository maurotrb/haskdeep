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
    ,OptExecution(..)
    ,OptCompMode(..)
    ,optionsPI
    )
where

import           Prelude hiding (FilePath)

import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC
import           Options.Applicative

data Options = Options OptExecution OptCompMode FilePath FilePath

data OptExecution = OptComputation
                  | OptAudit

data OptCompMode = OptMD5
                 | OptSHA1
                 | OptSHA256
                 | OptSkein512

fpReader :: String -> Maybe FilePath
fpReader fp = Just $ FSC.decodeString fp

compReader :: String -> Maybe OptCompMode
compReader "md5"      = Just OptMD5
compReader "sha1"     = Just OptSHA1
compReader "sha256"   = Just OptSHA256
compReader "skein512" = Just OptSkein512
compReader _          = Nothing

optionsP :: Parser Options
optionsP = Options
           <$> flag OptComputation OptAudit
                   ( long "audit"
                     & short 'a'
                     & help "Audit" )
            <*> nullOption
                    ( long "computation"
                      & short 'c'
                      & metavar "MODE"
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
