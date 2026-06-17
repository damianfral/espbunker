{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunkerSpec where

import Control.Monad.Indexed ((>>>=))
import Control.Monad.Indexed.Free (iliftFree)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import ESPBunker (generateYAML)
import ESPBunker.Actions
import ESPBunker.Components
import ESPBunker.DSL (Node (NodeArray, NodeObject), nodesToKeyMap)
import ESPBunker.Examples
import Path
import Relude
import System.Process.Typed (ExitCode (ExitSuccess), runProcess)
import Test.Syd
import Test.Syd.Path (tempDirSetupFunc)

setupSpec :: TestDefM outers (Path Abs Dir) result -> TestDefM outers any result
setupSpec = setupAround $ tempDirSetupFunc "espbunker-example"

spec :: Spec
spec = do
  describe "ESPBunker" $ setupSpec $ forM_ examples $ uncurry testExample

  describe "interpretAction" $ do
    it "noAction produces empty array" $ do
      interpretAction noAction `shouldBe` (empty :: Array)

    it "turnOn switch"
      $ let s = Switch :: Switch "my_switch" GPIO 5
         in interpretAction (turnOn s)
              `shouldBe` [object ["switch.turn_on" .= ("my_switch" :: Text)]]

    it "turnOff switch"
      $ let s = Switch :: Switch "my_switch" GPIO 5
         in interpretAction (turnOff s)
              `shouldBe` [object ["switch.turn_off" .= ("my_switch" :: Text)]]

    it "toggleSwitch"
      $ let s = Switch :: Switch "my_switch" GPIO 5
         in interpretAction (iliftFree $ ToggleSwitch @"my_switch" s ())
              `shouldBe` [object ["switch.toggle" .= ("my_switch" :: Text)]]

    it "turnOnL light"
      $ let l = Light :: Light "my_light" RGB
         in interpretAction (turnOnL l)
              `shouldBe` [object ["light.turn_on" .= ("my_light" :: Text)]]

    it "turnOffL light"
      $ let l = Light :: Light "my_light" RGB
         in interpretAction (turnOffL l)
              `shouldBe` [object ["light.turn_off" .= ("my_light" :: Text)]]

    it "log message" $ do
      interpretAction (log "hello")
        `shouldBe` [object ["logger.log" .= ("hello" :: Text)]]

    it "delay milliseconds" $ do
      interpretAction (delay 1500)
      `shouldBe` [object ["delay" .= ("1500ms" :: Text)]]

    it "componentUpdate" $ do
      interpretAction (componentUpdate "my_sensor")
      `shouldBe` [object ["component.update" .= ("my_sensor" :: Text)]]

    it "componentSuspend"
      $ interpretAction (componentSuspend "my_sensor")
      `shouldBe` [object ["component.suspend" .= ("my_sensor" :: Text)]]

    it "runScript"
      $ let sc = Script :: Script "my_script"
         in interpretAction (runScript sc)
              `shouldBe` [object ["script.execute" .= ("my_script" :: Text)]]

    it "setNumber" $ do
      let n = NumberComponent :: NumberComponent "my_number"
          expectedObject =
            object
              [ "number.set"
                  .= object
                    [ "id" .= ("my_number" :: Text),
                      "value" .= (42.5 :: Double)
                    ]
              ]
       in interpretAction (iliftFree $ SetNumber @"my_number" n 42.5 ())
            `shouldBe` [expectedObject]

    it "incrementNumber"
      $ let n = NumberComponent :: NumberComponent "my_number"
         in interpretAction (iliftFree $ IncrementNumber @"my_number" n 1.0 ())
              `shouldBe` [object ["number.increment" .= ("my_number" :: Text)]]

    it "decrementNumber"
      $ let n = NumberComponent :: NumberComponent "my_number"
         in interpretAction (iliftFree $ DecrementNumber @"my_number" n 1.0 ())
              `shouldBe` [object ["number.decrement" .= ("my_number" :: Text)]]

    it "openCover"
      $ let c = Cover :: Cover "my_cover" Endstop
         in interpretAction (iliftFree $ OpenCover @"my_cover" c ())
              `shouldBe` [object ["cover.open" .= ("my_cover" :: Text)]]

    it "closeCover"
      $ let c = Cover :: Cover "my_cover" Endstop
         in interpretAction (iliftFree $ CloseCover @"my_cover" c ())
              `shouldBe` [object ["cover.close" .= ("my_cover" :: Text)]]

    it "stopCover"
      $ let c = Cover :: Cover "my_cover" Endstop
         in interpretAction (iliftFree $ StopCover @"my_cover" c ())
              `shouldBe` [object ["cover.stop" .= ("my_cover" :: Text)]]

    it "sampleSensor"
      $ let s = Sensor :: Sensor "my_sensor"
         in interpretAction (iliftFree $ SampleSensor @"my_sensor" s ())
              `shouldBe` [object ["component.update" .= ("my_sensor" :: Text)]]

    it "sampleTextSensor" $ do
      let s = TextSensor :: TextSensor "my_text_sensor"
          expectedObject =
            object ["component.update" .= ("my_text_sensor" :: Text)]
      interpretAction (iliftFree $ SampleTextSensor @"my_text_sensor" s ())
        `shouldBe` [expectedObject]

    it "setOutputValue" $ do
      let o = Output :: Output "my_output" GPIO
          expectedObject =
            object
              [ "output.set_level"
                  .= object
                    ["id" .= ("my_output" :: Text), "level" .= (0.75 :: Double)]
              ]
      interpretAction (iliftFree $ SetOutputValue @"my_output" o 0.75 ())
        `shouldBe` [expectedObject]

    it "turnOnOutput"
      $ let o = Output :: Output "my_output" GPIO
         in interpretAction (iliftFree $ TurnOnOutput @"my_output" o ())
              `shouldBe` [object ["output.turn_on" .= ("my_output" :: Text)]]

    it "turnOffOutput"
      $ let o = Output :: Output "my_output" GPIO
         in interpretAction (iliftFree $ TurnOffOutput @"my_output" o ())
              `shouldBe` [object ["output.turn_off" .= ("my_output" :: Text)]]

    it "ifSwitchIsOn with then only"
      $ let s = Switch :: Switch "my_switch" GPIO 5
            action = ifSwitchIsOn s (log "on") Nothing
            expectedObject =
              object
                [ "if"
                    .= object
                      [ "condition"
                          .= object ["switch.is_on" .= ("my_switch" :: Text)],
                        "then"
                          .= Array [object ["logger.log" .= ("on" :: Text)]]
                      ]
                ]
         in interpretAction action `shouldBe` [expectedObject]

    it "ifSwitchIsOn with then and else"
      $ let s = Switch :: Switch "my_switch" GPIO 5
            action = ifSwitchIsOn s (log "on") (Just (log "off"))
            expectedObject =
              object
                [ "if"
                    .= object
                      [ "condition"
                          .= object ["switch.is_on" .= ("my_switch" :: Text)],
                        "then"
                          .= Array [object ["logger.log" .= ("on" :: Text)]],
                        "else"
                          .= Array [object ["logger.log" .= ("off" :: Text)]]
                      ]
                ]
         in interpretAction action `shouldBe` [expectedObject]

    it "ifSwitchIsOff with then only" $ do
      let s = Switch :: Switch "my_switch" GPIO 5
          action = ifSwitchIsOff s (log "off") Nothing
          expectedObject =
            object
              [ "if"
                  .= object
                    [ "condition"
                        .= object ["switch.is_off" .= ("my_switch" :: Text)],
                      "then"
                        .= Array [object ["logger.log" .= ("off" :: Text)]]
                    ]
              ]
      interpretAction action `shouldBe` [expectedObject]

    it "sequential actions produce combined array"
      $ let s = Switch :: Switch "my_switch" GPIO 5
            both = turnOn s >>>= \_ -> turnOff s
         in interpretAction both
              `shouldBe` [ object ["switch.turn_on" .= ("my_switch" :: Text)],
                           object ["switch.turn_off" .= ("my_switch" :: Text)]
                         ]

    it "logger.log with special characters" $ do
      interpretAction (log "temperature is 25°C")
      `shouldBe` [object ["logger.log" .= ("temperature is 25°C" :: Text)]]

    it "delay zero" $ do
      interpretAction (delay 0) `shouldBe` [object ["delay" .= ("0ms" :: Text)]]

    it "setNumber with zero" $ do
      let n = NumberComponent :: NumberComponent "my_number"
          expectedObject =
            object
              [ "number.set"
                  .= object
                    ["id" .= ("my_number" :: Text), "value" .= (0 :: Double)]
              ]
      interpretAction (iliftFree $ SetNumber @"my_number" n 0 ())
        `shouldBe` [expectedObject]

  describe "nodesToKeyMap" $ do
    it "empty list produces empty KeyMap" $ do
      nodesToKeyMap [] `shouldBe` KM.empty

    it "single NodeObject" $ do
      let node = NodeObject "esphome" $ KM.singleton "name" (String "test")
          expected =
            KM.singleton "esphome"
              $ Object (KM.singleton "name" (String "test"))
      nodesToKeyMap [node] `shouldBe` expected

    it "single NodeArray" $ do
      let km = KM.singleton "platform" (String "gpio")
          node = NodeArray "switch" (km :| [])
          expected = KM.singleton "switch" (Array [Object km])
      nodesToKeyMap [node] `shouldBe` expected

    it "multiple nodes with different keys"
      $ let espKM = KM.singleton "name" (String "test")
            wifiKM = KM.singleton "ssid" (String "mywifi")
            espNode = NodeObject "esphome" espKM
            wifiNode = NodeObject "wifi" wifiKM
            expected =
              KM.fromList
                [ ("esphome", Object espKM),
                  ("wifi", Object wifiKM)
                ]
         in nodesToKeyMap [espNode, wifiNode] `shouldBe` expected

    it "two NodeArrays with same key concatenates (newest first)"
      $ let km1 = KM.singleton "id" (String "sw1")
            km2 = KM.singleton "id" (String "sw2")
            node1 = NodeArray "switch" (km1 :| [])
            node2 = NodeArray "switch" (km2 :| [])
            expected = KM.singleton "switch" (Array [Object km2, Object km1])
         in nodesToKeyMap [node1, node2] `shouldBe` expected

    it "three NodeArrays with same key concatenates (newest first)"
      $ let km1 = KM.singleton "id" (String "a")
            km2 = KM.singleton "id" (String "b")
            km3 = KM.singleton "id" (String "c")
            node1 = NodeArray "items" (km1 :| [])
            node2 = NodeArray "items" (km2 :| [])
            node3 = NodeArray "items" (km3 :| [])
            expected =
              KM.singleton "items" $ Array [Object km3, Object km2, Object km1]
         in nodesToKeyMap [node1, node2, node3] `shouldBe` expected

    it "NodeObject then NodeArray with same key: object wins (deepMerge _ v2 = v2)"
      $ let objKM = KM.singleton "name" (String "original")
            arrKM = KM.singleton "id" (String "new")
            objNode = NodeObject "x" objKM
            arrNode = NodeArray "x" (arrKM :| [])
            expected = KM.singleton "x" (Object objKM)
         in nodesToKeyMap [objNode, arrNode] `shouldBe` expected

    it "NodeArray then NodeObject with same key: object wins (insert overwrites)"
      $ let arrKM = KM.singleton "id" (String "first")
            objKM = KM.singleton "name" (String "second")
            arrNode = NodeArray "x" (arrKM :| [])
            objNode = NodeObject "x" objKM
            expected = KM.singleton "x" (Object objKM)
         in nodesToKeyMap [arrNode, objNode] `shouldBe` expected

    it "interleaved keys merge independently"
      $ let kmA1 = KM.singleton "id" $ String "a1"
            kmA2 = KM.singleton "id" $ String "a2"
            kmB = KM.singleton "name" $ String "b"
            nodes =
              [ NodeArray "switches" (kmA1 :| []),
                NodeObject "config" kmB,
                NodeArray "switches" (kmA2 :| [])
              ]
            expected =
              KM.fromList
                [ ("switches", Array [Object kmA2, Object kmA1]),
                  ("config", Object kmB)
                ]
         in nodesToKeyMap nodes `shouldBe` expected

testExample :: Text -> SomeESPM -> TestDefM outers (Path b Dir) ()
testExample name (SomeESPM example) = do
  it ("produces valid ESPHome YAML files for example: " <> toString name)
    $ \dir -> do
      let fp = dir </> [relfile|example.yaml|]
      writeFileBS (toFilePath fp) $ generateYAML example
      code <- runProcess $ fromString $ "esphome config " <> toFilePath fp
      code `shouldBe` ExitSuccess
