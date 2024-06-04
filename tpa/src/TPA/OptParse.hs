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
import System.Directory (doesDirectoryExist, doesFileExist)
import TPA.Key

getSettings :: IO Settings
getSettings = runParser $ do
  let configFileParser =
        optionalFirst
          [ optional $
              strOption
                [ long "config-file",
                  help "Configuration file path"
                ],
            optional $
              envVar
                str
                [ var "CONFIG_FILE",
                  help "Configuration file path"
                ],
            optional $ xdgYamlConfigFile "tpa"
          ]
  withYamlConfig configFileParser $ do
    setFilter <-
      optional $
        strArgument
          [ metavar "QUERY",
            help "Query for the name of the keys to show"
          ]
    setKeys <- mapIO resolveKeys $ do
      flagPaths <-
        many $
          strOption
            [ long "path",
              help "Path to key files, either files or directories",
              metavar "PATH"
            ]

      configPaths <- confVal "keys-paths" :: Parser [FilePath]
      pure $ flagPaths ++ configPaths
    pure Settings {..}

-- | A product type for the settings that your program will use
data Settings = Settings
  { setFilter :: !(Maybe Text),
    setKeys :: ![Key]
  }

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
