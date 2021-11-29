{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TPA.Key where

import Crypto.Hash (SHA1 (..))
import Crypto.OTP
import Data.ByteString
import Data.ByteString.Base32 as Base32
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Yaml
import GHC.Generics (Generic)

data Key
  = Key
      { keyName :: !Text,
        keySecret :: !Secret
      }
  deriving (Show, Eq, Generic)

instance FromJSON Key where
  parseJSON = withObject "Key" $ \o ->
    Key
      <$> o .: "name"
      <*> o .: "secret"

otpForKey :: OTPTime -> Key -> Either String OTP
otpForKey time Key {..} = do
  params <-
    mkTOTPParams
      SHA1
      0 -- Epoch time
      30 -- Time step
      OTP6
      TwoSteps
  pure $ totp params (unSecret keySecret) time

getOTPTime :: IO OTPTime
getOTPTime = floor <$> getPOSIXTime

newtype Secret = Secret {unSecret :: ByteString}
  deriving (Show, Eq, Generic)

instance FromJSON Secret where
  parseJSON = withText "Secret" $ \t ->
    case decodeBase32Unpadded (TE.encodeUtf8 t) of
      Left err -> fail $ T.unpack err
      Right bs -> pure $ Secret bs
