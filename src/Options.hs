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
  ( Options (..),
    OptExecution (..),
    OptCompMode (..),
    optionsPI,
    haskdeepVersion,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time.Format as DTF
import HaskDeep
import Options.Applicative
import Prelude hiding (FilePath)

haskdeepVersion :: String
haskdeepVersion = "haskdeep 0.2.1.0 - file hashing and audit"

data Options
  = Version
  | Options OptExecution OptCompMode HaskDeepConfiguration

data OptExecution
  = OptComputation
  | OptAudit

data OptCompMode
  = OptMD5
  | OptSHA1
  | OptSHA256
  | OptSkein512

optionsPI :: ParserInfo Options
optionsPI =
  info
    (helper <*> optionsP)
    ( fullDesc
        <> progDesc "Computes hashes and audit a set of files"
        <> header haskdeepVersion
    )

configurationP :: Parser HaskDeepConfiguration
configurationP =
  HaskDeepConfiguration
    <$> strOption
      ( long "root"
          <> short 'r'
          <> metavar "DIRNAME"
          <> help "Root directory - default current directory"
          <> value (rootDirectory defaultHaskDeepConfiguration)
      )
    <*> strOption
      ( long "known"
          <> short 'k'
          <> metavar "FILENAME"
          <> help "Known hashes file - default known.haskdeep"
          <> value (knownHashes defaultHaskDeepConfiguration)
      )
    <*> option
      (str >>= parseText)
      ( long "excl-regex"
          <> short 'e'
          <> metavar "REGEX"
          <> help "Exclude files or directories based on regex"
          <> value (excludeRegex defaultHaskDeepConfiguration)
          <> hidden
      )
    <*> option
      (str >>= parseTime)
      ( long "incl-mod-from"
          <> short 'f'
          <> metavar "DATE"
          <> help "Include files modified from yyyy-mm-ddThh:mm:ssZ"
          <> value (includeModFrom defaultHaskDeepConfiguration)
          <> hidden
      )
    <*> option
      (str >>= parseTime)
      ( long "incl-mod-upto"
          <> short 't'
          <> metavar "DATE"
          <> help "Include files modified up to yyyy-mm-ddThh:mm:ssZ"
          <> value (includeModUpTo defaultHaskDeepConfiguration)
          <> hidden
      )

optionsP :: Parser Options
optionsP =
  flag'
    Version
    ( long "version"
        <> short 'v'
        <> help "Show version information"
        <> hidden
    )
    <|> ( Options
            <$> subparser
              ( command
                  "compute"
                  ( info
                      (pure OptComputation)
                      (progDesc "Computes file hashes and saves them to known hashes file")
                  )
                  <> command
                    "audit"
                    ( info
                        (pure OptAudit)
                        (progDesc "Audit files comparing them to known hashes")
                    )
              )
            <*> option
              (str >>= parseOptCompMode)
              ( long "computation"
                  <> short 'c'
                  <> metavar "MODE"
                  <> help "md5 | sha1 | sha256 | skein512 - default md5"
                  <> value OptMD5
              )
            <*> configurationP
        )

parseOptCompMode :: String -> ReadM OptCompMode
parseOptCompMode "md5" = return OptMD5
parseOptCompMode "sha1" = return OptSHA1
parseOptCompMode "sha256" = return OptSHA256
parseOptCompMode "skein512" = return OptSkein512
parseOptCompMode _ = readerAbort (ShowHelpText mempty)

parseText :: String -> ReadM (Maybe Text)
parseText s = return $ Just $ T.pack s

parseTime :: String -> ReadM (Maybe UTCTime)
parseTime s = case parsedTime of
  (Just _) -> return parsedTime
  Nothing -> readerAbort (ShowHelpText mempty)
  where
    parsedTime = DTF.parseTimeM True DTF.defaultTimeLocale "%FT%TZ" s
