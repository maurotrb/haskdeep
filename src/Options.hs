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
    ,haskdeepVersion
    )
where

import           Prelude hiding (FilePath)

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Data.Time as DT
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FSC
import           Options.Applicative
import qualified System.Locale as SL

import           HaskDeep

haskdeepVersion :: String
haskdeepVersion = "haskdeep 0.1.0.0 - file hashing and audit"

data Options = Version
             | Options OptExecution OptCompMode HaskDeepConfiguration

data OptExecution = OptComputation
                  | OptAudit

data OptCompMode = OptMD5
                 | OptSHA1
                 | OptSHA256
                 | OptSkein512

optionsPI :: ParserInfo Options
optionsPI = info (helper <*> optionsP)
            ( fullDesc
              <> progDesc "Computes hashes and audit a set of files"
              <> header haskdeepVersion)

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
                         ( long "excl-regex"
                           <> short 'e'
                           <> metavar "REGEX"
                           <> help "Exclude files or directories based on regex"
                           <> reader regexReader
                           <> value (excludeRegex defaultHaskDeepConfiguration)
                           <> hidden )
                 <*> nullOption
                         ( long "incl-mod-from"
                           <> short 'f'
                           <> metavar "DATE"
                           <> help "Include files modified from yyyy-mm-ddThh:mm:ssZ"
                           <> reader timeReader
                           <> value (includeModFrom defaultHaskDeepConfiguration)
                           <> hidden )
                 <*> nullOption
                         ( long "incl-mod-upto"
                           <> short 't'
                           <> metavar "DATE"
                           <> help "Include files modified up to yyyy-mm-ddThh:mm:ssZ"
                           <> reader timeReader
                           <> value (includeModUpTo defaultHaskDeepConfiguration)
                           <> hidden )

optionsP :: Parser Options
optionsP = flag' Version
                   ( long "version"
                     <> short 'v'
                     <> help "Show version information"
                     <> hidden )
           <|> ( Options
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
                 <*> configurationP )

compReader :: String -> Either ParseError OptCompMode
compReader "md5"      = Right OptMD5
compReader "sha1"     = Right OptSHA1
compReader "sha256"   = Right OptSHA256
compReader "skein512" = Right OptSkein512
compReader _          = Left ShowHelpText

fpReader :: String -> Either ParseError FilePath
fpReader fp = Right $ FSC.decodeString fp

regexReader :: String -> Either ParseError (Maybe Text)
regexReader = Right . Just . T.pack

timeReader :: String -> Either ParseError (Maybe UTCTime)
timeReader dt = case parsedTime of
                  (Just _) -> Right parsedTime
                  Nothing  -> Left ShowHelpText
    where
      parsedTime = DT.parseTime SL.defaultTimeLocale "%FT%TZ" dt
