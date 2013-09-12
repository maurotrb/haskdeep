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

module Options
    (
     Options(..)
    ,OptExecution(..)
    ,OptCompMode(..)
    ,optionsPI
    )
where

import           Prelude hiding (FilePath)

import           Data.Text (Text)
import qualified Data.Text as T
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC
import           Options.Applicative

import           HaskDeep

data Options = Options OptExecution OptCompMode HaskDeepConfiguration

data OptExecution = OptComputation
                  | OptAudit

data OptCompMode = OptMD5
                 | OptSHA1
                 | OptSHA256
                 | OptSkein512

optionsPI :: ParserInfo Options
optionsPI = info (optionsP <**> helper)
            ( fullDesc
              <> progDesc "Computes hashes and audit a set of files"
              <> header "haskdeep - file hashing and audit" )

configurationP :: Parser HaskDeepConfiguration
configurationP = HaskDeepConfiguration
                 <$> nullOption
                         ( long "root"
                           <> short 'r'
                           <> metavar "DIRNAME"
                           <> help "Root directory - default current directory"
                           <> reader fpReader
                           <> value (rootDirectory defaultHaskDeepConfiguration))
                 <*> nullOption
                         ( long "known"
                           <> short 'k'
                           <> metavar "FILENAME"
                           <> help "Known hashes file - default known.haskdeep"
                           <> reader fpReader
                           <> value (knownHashes defaultHaskDeepConfiguration))
                 <*> nullOption
                         ( long "ignore"
                           <> short 'i'
                           <> metavar "RULE"
                           <> help "Regex to ignore files or directories"
                           <> reader ignReader
                           <> value (ignoreRule defaultHaskDeepConfiguration))

optionsP :: Parser Options
optionsP = Options
           <$> subparser
                   ( command "compute"
                     (info (pure OptComputation)
                               (progDesc "Computes file hashes and saves them to known hashes file"))
                     <> command "audit"
                     (info (pure OptAudit)
                               (progDesc "Audit files comparing them to known hashes")))
           <*> nullOption
                   ( long "computation"
                     <> short 'c'
                     <> metavar "MODE"
                     <> help "md5 | sha1 | sha256 | skein512 - default md5"
                     <> reader compReader
                     <> value OptMD5)
           <*> configurationP

fpReader :: String -> Either ParseError FilePath
fpReader fp = Right $ FSC.decodeString fp

ignReader :: String -> Either ParseError (Maybe Text)
ignReader = Right . Just . T.pack

compReader :: String -> Either ParseError OptCompMode
compReader "md5"      = Right OptMD5
compReader "sha1"     = Right OptSHA1
compReader "sha256"   = Right OptSHA256
compReader "skein512" = Right OptSkein512
compReader _          = Left ShowHelpText
