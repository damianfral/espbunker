{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunkerSpec where

import ESPBunker (generateYAML)
import ESPBunker.Examples
import Path
import Relude
import System.Process.Typed (ExitCode (ExitSuccess), runProcess)
import Test.Syd
import Test.Syd.Path (tempDirSetupFunc)

setupSpec :: TestDefM outers (Path Abs Dir) result -> TestDefM outers any result
setupSpec = setupAround $ tempDirSetupFunc "espbunker-example"

spec :: Spec
spec = setupSpec $ describe "ESPBunker" $ forM_ examples $ uncurry testExample

testExample :: Text -> SomeESPM -> TestDefM outers (Path b Dir) ()
testExample name (SomeESPM example) = do
  it ("produces valid ESPHome YAML files for example: " <> toString name)
    $ \dir -> do
      let fp = dir </> [relfile|example.yaml|]
      writeFileBS (toFilePath fp) $ generateYAML example
      code <- runProcess $ fromString $ "esphome config " <> toFilePath fp
      code `shouldBe` ExitSuccess
