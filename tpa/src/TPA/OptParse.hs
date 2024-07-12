{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TPA.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Control.Applicative
import Data.Text (Text)
import Data.Yaml as Yaml (decodeFileThrow)
import OptEnvConf
import Path
import Path.IO hiding (doesFileExist)
import Paths_tpa (version)
import System.Directory (doesDirectoryExist, doesFileExist)
import TPA.Key

getSettings :: IO Settings
getSettings = runSettingsParser version "Third party authenticator"

data Settings = Settings
  { setFilter :: !(Maybe Text),
    setKeys :: ![Key]
  }

instance HasParser Settings where
  settingsParser =
    withConfigurableYamlConfig (xdgYamlConfigFile "tpa") $ do
      setFilter <-
        optional $
          setting
            [ help "Query for the name of the keys to show",
              reader str,
              argument,
              metavar "QUERY"
            ]
      setKeys <- mapIO resolveKeys $ do
        flagPaths <-
          many $
            setting
              [ help "Path to key files, either files or directories",
                reader str,
                option,
                long "path",
                metavar "PATH"
              ]

        configPaths <-
          setting
            [ help "key paths",
              conf "key-paths",
              value []
            ] ::
            Parser [FilePath]
        pure $ flagPaths ++ configPaths
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
