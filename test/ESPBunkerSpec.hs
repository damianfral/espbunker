{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ESPBunkerSpec where

import ESPBunker.Examples
import ESPBunker.Language (Board, ESPM, generateYAML)
import GHC.TypeLits
import Path
import Relude
import System.Process.Typed (ExitCode (ExitSuccess), runProcess)
import Test.Syd
import Test.Syd.Path (tempDirSetupFunc)

setupSpec :: TestDefM outers (Path Abs Dir) result -> TestDefM outers any result
setupSpec = setupAround $ tempDirSetupFunc "espbunker-example"

spec :: Spec
spec = setupSpec $ do
  describe "ESPBunker" $ do
    -- testExample binaryOutputExample
    -- testExample buttonExample
    testExample "cover" coverExample
    testExample "light" lightExample
    testExample "output" outputExample
    testExample "sensor" sensorExample
    testExample "switch" switchExample

-- testExample scriptExample
-- testExample numberExample
-- testExample selectExample
-- testExample outputGPIOExample
-- testExample lightOutExample
testExample ::
  (KnownSymbol boardName) =>
  String ->
  ESPM (Board boardName boardNames boardPins) board' () ->
  TestDefM outers (Path b Dir) ()
testExample name example = do
  it ("produces valid ESPHome YAML files for example: " <> name) $ \dir -> do
    let fp = dir </> [relfile|example.yaml|]
    writeFileBS (toFilePath fp) $ generateYAML example
    code <- runProcess $ fromString $ "esphome config " <> toFilePath fp
    code `shouldBe` ExitSuccess
