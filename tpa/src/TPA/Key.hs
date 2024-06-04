{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TPA.Key where

import Autodocodec
import Control.Arrow (left)
import Crypto.Hash (SHA1 (..))
import Crypto.OTP
import Data.ByteString
import Data.ByteString.Base32 as Base32
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Yaml (FromJSON)
import Text.Printf

data Key = Key
  { keyName :: !Text,
    keySecret :: !Secret
  }
  deriving (FromJSON) via (Autodocodec Key)

instance HasCodec Key where
  codec =
    object "Key" $
      Key
        <$> requiredField "name" "name of the key, for lookups" .= keyName
        <*> requiredField "secret" "OTP secret" .= keySecret

otpForKey :: OTPTime -> Key -> Either String String
otpForKey time Key {..} = do
  let digits = OTP6
  params <-
    mkTOTPParams
      SHA1
      0 -- Epoch time
      30 -- Time step
      digits
      TwoSteps
  let showFunc :: OTP -> String
      showFunc = printf $ case digits of
        OTP4 -> "%04d"
        OTP5 -> "%05d"
        OTP6 -> "%06d"
        OTP7 -> "%07d"
        OTP8 -> "%08d"
        OTP9 -> "%09d"
  pure $ showFunc $ totp params (unSecret keySecret) time

getOTPTime :: IO OTPTime
getOTPTime = floor <$> getPOSIXTime

newtype Secret = Secret {unSecret :: ByteString}

instance HasCodec Secret where
  codec =
    bimapCodec
      (fmap Secret . left T.unpack . decodeBase32Unpadded . TE.encodeUtf8)
      (encodeBase32Unpadded . unSecret)
      codec
