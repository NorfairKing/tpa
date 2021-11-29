{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module TPA.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO hiding (doesFileExist)
import System.Directory (doesDirectoryExist, doesFileExist)
import TPA.Key

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

-- | A product type for the settings that your program will use
data Settings = Settings
  { setKeys :: [Key]
  }
  deriving (Show, Eq, Generic)

-- | Combine everything to 'Settings'
combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  setKeys <-
    resolveKeys $
      concat
        [ flagPaths,
          maybe id (:) envKeyPath (fromMaybe [] envKeyPaths),
          maybe [] configKeyPaths mConf
        ]
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

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
    goFile af = fromMaybe [] <$> readYamlConfigFile af

data Configuration = Configuration
  { configKeyPaths :: [FilePath]
  }
  deriving (Show, Eq, Generic)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration <$> requiredField "key-paths" "Paths to find key files. These can be both files and directories." .= configKeyPaths

-- | Get the configuration
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

-- | Where to get the configuration file by default.
--
-- This uses the XDG base directory specifictation:
-- https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|tpa|])
  resolveFile xdgConfigDir "config.yaml"

-- | What we find in the configuration variable.
data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envKeyPath :: Maybe FilePath,
    envKeyPaths :: Maybe [FilePath]
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FOO_BAR_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.str) "PATH" (mE <> Env.help "key path")
      <*> Env.var (fmap (Just . map (T.unpack . T.strip) . T.splitOn "," . T.pack) . Env.str) "PATHS" (mE <> Env.help "key paths, comma separated")
  where
    mE = Env.def Nothing <> Env.keep

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
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: Maybe FilePath,
    flagPaths :: [FilePath]
  }
  deriving (Show, Eq, Generic)

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
    <*> many
      ( strOption
          ( mconcat
              [ long "path",
                help "Path to key files, either files or directories",
                metavar "PATH"
              ]
          )
      )
