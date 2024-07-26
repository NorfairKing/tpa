{-# LANGUAGE TypeApplications #-}

module Tpa.OptParseSpec (spec) where

import OptEnvConf.Test
import TPA.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "tpa"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
