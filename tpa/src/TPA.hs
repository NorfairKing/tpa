{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TPA
  ( cli,
  )
where

import Data.List
import qualified Data.Text as T
import System.Exit
import TPA.Key
import TPA.OptParse

cli :: IO ()
cli = do
  Settings {..} <- getSettings
  now <- getOTPTime
  case setFilter of
    Nothing -> do
      let keyLine k@Key {..} =
            case otpForKey now k of
              Left err -> unwords [T.unpack keyName <> ":", err]
              Right otp -> unwords [show otp, T.unpack keyName]
      putStr $ unlines $ map keyLine setKeys
    Just nameFilter -> do
      case find ((== nameFilter) . keyName) setKeys of
        Nothing -> die $ "Key not found: " <> show nameFilter
        Just k -> case otpForKey now k of
          Left err -> die err
          Right otp -> print otp
