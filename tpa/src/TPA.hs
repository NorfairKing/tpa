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
  let filterFunc = case setFilter of
        Nothing -> id
        Just nameFilter -> filter ((nameFilter `T.isInfixOf`) . keyName)
  let filteredResults = filterFunc $ sortOn keyName setKeys
  case filteredResults of
    [] -> die "No keys found."
    [key] -> case otpForKey now key of
      Left err -> die err
      Right otp -> putStrLn otp
    keys -> do
      let keyLine k@Key {..} =
            case otpForKey now k of
              Left err -> unwords [T.unpack keyName <> ":", err]
              Right otp -> unwords [otp, T.unpack keyName]
      putStr $ unlines $ map keyLine keys
