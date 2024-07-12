{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TPA
  ( cli,
  )
where

import Control.Concurrent
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import System.Exit
import TPA.Key
import TPA.OptParse

cli :: IO ()
cli = do
  Settings {..} <- getSettings
  now <- getOTPTime
  let filterFunc = case setFilter of
        Nothing -> id
        Just nameFilter -> filter ((T.toCaseFold nameFilter `T.isInfixOf`) . T.toCaseFold . keyName)
  case NE.nonEmpty $ filterFunc $ sortOn keyName setKeys of
    Nothing -> die "No keys found."
    Just filteredKeys -> do
      let output =
            case filteredKeys of
              key :| [] -> case otpForKey now key of
                Left err -> die err
                Right otp -> putStrLn otp
              keys -> do
                let keyLine k@Key {..} =
                      case otpForKey now k of
                        Left err -> unwords [T.unpack keyName <> ":", err]
                        Right otp -> unwords [otp, T.unpack keyName]
                putStr $ unlines $ map keyLine $ NE.toList keys

      let watch :: IO () -> IO ()
          watch func = do
            putStr "\x1b[2J\x1b[H"
            func
            threadDelay 1_000_000
            watch func
      if setWatch
        then watch output
        else output
