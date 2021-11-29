{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TPA.Key where

import Autodocodec
import Control.Arrow (left)
import Data.ByteString
import Data.ByteString.Base32 as Base32
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

data Key = Key
  { keyName :: !Text,
    keySecret :: !Secret
  }
  deriving (Show, Eq, Generic)

instance HasCodec Key where
  codec =
    object "Key" $
      Key
        <$> requiredField "name" "name of the key; example: github" .= keyName
        <*> requiredField "secret" "secret key, base32 encoded" .= keySecret

newtype Secret = Secret {unSecret :: ByteString}
  deriving (Show, Eq, Generic)

instance HasCodec Secret where
  codec =
    dimapCodec Secret unSecret $
      bimapCodec
        (left T.unpack . decodeBase32Unpadded . TE.encodeUtf8)
        encodeBase32Unpadded
        codec
