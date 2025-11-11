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
    testExample 1 example1
    testExample 2 example2
    testExample 3 example3
    testExample 4 example4

testExample ::
  (KnownSymbol boardName) =>
  Int ->
  ESPM (Board boardName boardNames boardPins) board' () ->
  TestDefM outers (Path b Dir) ()
testExample i example = do
  it ("produces valid ESPHome YAML files for example" <> show i) $ \dir -> do
    let fp = dir </> [relfile|example.yaml|]
    writeFileBS (toFilePath fp) $ generateYAML example
    code <- runProcess $ fromString $ "esphome config " <> toFilePath fp
    code `shouldBe` ExitSuccess
