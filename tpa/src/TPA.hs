{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TPA
  ( cli,
  )
where

import Crypto.Hash (SHA1 (..))
import Crypto.OTP
import Data.ByteString (ByteString)
import Data.ByteString.Base32 as Base32
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import System.Exit
import TPA.Key
import TPA.OptParse

cli :: IO ()
cli = do
  Settings {..} <- getSettings
  otpTime <- getOTPTime
  case mkTOTPParams SHA1 0 30 OTP6 TwoSteps of
    Left err -> die err
    Right params -> do
      let keyLine Key {..} =
            let otp = totp params (unSecret keySecret) otpTime
             in concat [T.unpack keyName, ": ", show otp]
      putStr $ unlines $ map keyLine setKeys

getOTPTime :: IO OTPTime
getOTPTime = getPOSIXTime >>= \t -> return (floor t)
