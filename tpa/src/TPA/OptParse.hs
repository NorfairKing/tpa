{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module TPA.OptParse where

import Control.Applicative
import Data.Text (Text)
import Data.Yaml as Yaml
import Options.Applicative as OptParse
import Path
import Path.IO hiding (doesFileExist)
import System.Directory (doesDirectoryExist, doesFileExist)
import TPA.Key

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  config <- getConfiguration flags
  combineToSettings flags config

-- | A product type for the settings that your program will use
data Settings = Settings
  { setFilter :: !(Maybe Text),
    setKeys :: ![Key]
  }

-- | Combine everything to 'Settings'
combineToSettings :: Flags -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} mConf = do
  let setFilter = flagFilter
  setKeys <-
    resolveKeys $
      concat
        [ flagPaths,
          maybe [] configKeyPaths mConf
        ]
  pure Settings {..}

resolveKeys :: [FilePath] -> IO [Key]
resolveKeys = fmap concat . mapM go
  where
    go :: FilePath -> IO [Key]
    go p = do
      isFile <- doesFileExist p
      if isFile
        then resolveFile' p >>= goFile
        else do
          isDir <- doesDirectoryExist p
          if isDir
            then resolveDir' p >>= goDir
            else pure []
    goDir :: Path Abs Dir -> IO [Key]
    goDir ad = do
      files <- snd <$> listDirRecur ad
      let rightExtension af = fileExtension af == Just ".password"
      concat <$> mapM goFile (filter rightExtension files)
    goFile :: Path Abs File -> IO [Key]
    goFile = Yaml.decodeFileThrow . fromAbsFile

data Configuration = Configuration
  { configKeyPaths :: ![FilePath]
  }

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o ->
    Configuration
      <$> o .:? "key-paths" .!= []

-- | Get the configuration
getConfiguration :: Flags -> IO (Maybe Configuration)
getConfiguration Flags {..} =
  case flagConfigFile of
    Nothing -> defaultConfigFile >>= forgivingAbsence . decodeFileThrow . fromAbsFile
    Just cf -> do
      afp <- resolveFile' cf
      Just <$> decodeFileThrow (fromAbsFile afp)

-- | Where to get the configuration file by default.
--
-- This uses the XDG base directory specifictation:
-- https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|tpa|])
  resolveFile xdgConfigDir "config.yaml"

-- | Get the command-line flags
getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Flags'
flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    OptParse.fullDesc

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagFilter :: !(Maybe Text),
    flagPaths :: ![FilePath]
  }

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( strArgument
          ( mconcat
              [ help "the key to show"
              ]
          )
      )
    <*> many
      ( strOption
          ( mconcat
              [ long "path",
                help "Path to key files, either files or directories",
                metavar "PATH"
              ]
          )
      )
