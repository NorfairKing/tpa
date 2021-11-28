{-# LANGUAGE OverloadedStrings #-}

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

cli :: IO ()
cli = do
  let testKey = "BOAZML22JBYSVGAA" :: ByteString
  case decodeBase32Unpadded testKey of
    Left err -> die $ T.unpack err
    Right key -> do
      let getOTPTime = getPOSIXTime >>= \t -> return (floor t :: OTPTime)
      posixT <- getOTPTime
      case mkTOTPParams SHA1 0 30 OTP6 TwoSteps of
        Left err -> die err
        Right params -> do
          let otp = totp params key posixT
          print otp
