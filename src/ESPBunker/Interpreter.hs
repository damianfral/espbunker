{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Interpreter where

import Control.Monad.Indexed.Free (IxFree (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Yaml (Value (..), encode, object)
import ESPBunker.Language
import GHC.TypeLits
import Relude hiding (natVal)

--------------------------------------------------------------------------------

-- * Action interpreter

-- Convert ESPAction to YAML automation steps
interpretAction :: IxFree ESPActionF i j () -> [Value]
interpretAction (Pure _) = []
interpretAction (Free espf) = case espf of
  TurnOnSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        pair = ("switch.turn_on", String (T.pack n))
     in object [pair] : interpretAction next
  TurnOffSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        pair = ("switch.turn_off", String (T.pack n))
     in object [pair] : interpretAction next
  TurnOnLight @name _light next ->
    let n = symbolVal (Proxy @name)
        pair = ("light.turn_on", String (T.pack n))
     in object [pair] : interpretAction next
  TurnOffLight @name _light next ->
    let n = symbolVal (Proxy @name)
        pair = ("light.turn_off", String (T.pack n))
     in object [pair] : interpretAction next
  LogMsg msg next ->
    let pair = ("logger.log", String msg)
     in object [pair] : interpretAction next
  Delay ms next ->
    let pair = ("delay", String (show ms <> "ms"))
     in object [pair] : interpretAction next
  RunScript @name _script next ->
    let n = symbolVal (Proxy @name)
        pair = ("script.execute", String (T.pack n))
     in object [pair] : interpretAction next
  SetNumber @name _number val next ->
    let n = symbolVal (Proxy @name)
        yamlNode =
          object
            [ ( "number.set",
                object
                  [ ("id", String (T.pack n)),
                    ("value", String (show val))
                  ]
              )
            ]
     in yamlNode : interpretAction next
  IncrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        pair = ("number.increment", String (T.pack n))
     in object [pair] : interpretAction next
  DecrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        pair = ("number.decrement", String (T.pack n))
     in object [pair] : interpretAction next
  SetOutputValue @name _output val next ->
    let n = symbolVal (Proxy @name)
        yamlNode =
          object
            [ ( "output.set_level",
                object
                  [ ("id", String (T.pack n)),
                    ("level", String (T.pack (show val)))
                  ]
              )
            ]
     in yamlNode : interpretAction next
  TurnOnOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        pair = ("output.turn_on", String (T.pack n))
     in object [pair] : interpretAction next
  TurnOffOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        pair = ("output.turn_off", String (T.pack n))
     in object [pair] : interpretAction next
  ToggleBinaryOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        pair = ("switch.toggle", String (T.pack n))
     in object [pair] : interpretAction next
  OpenCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        pair = ("cover.open", String (T.pack n))
     in object [pair] : interpretAction next
  CloseCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        pair = ("cover.close", String (T.pack n))
     in object [pair] : interpretAction next
  StopCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        pair = ("cover.stop", String (T.pack n))
     in object [pair] : interpretAction next
  SampleSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        pair = ("component.update", String (T.pack n))
     in object [pair] : interpretAction next
  SampleTextSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        pair = ("component.update", String (T.pack n))
     in object [pair] : interpretAction next

--------------------------------------------------------------------------------

-- * Main interpreter

interpretESP :: IxFree ESPF i j a -> [Value]
interpretESP (Pure _) = []
interpretESP (Free espf) =
  case espf of
    MkBoard next -> interpretESP next
    MkSwitch @name @pin next ->
      let n = symbolVal (Proxy @name)
          p = natVal (Proxy @pin)
          yamlNode =
            object
              [ ( "switch",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String (show p)),
                      ("name", String (T.pack n)),
                      ("id", String (T.pack n))
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkCover @name @open @close next ->
      let n = symbolVal (Proxy @name)
          o = natVal (Proxy @open)
          c = natVal (Proxy @close)
          open_output_id = T.pack (n <> "_open")
          close_output_id = T.pack (n <> "_close")
          openOutputNode =
            object
              [ ( "output",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String (show o)),
                      ("id", String open_output_id)
                    ]
                )
              ]
          closeOutputNode =
            object
              [ ( "output",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String (show c)),
                      ("id", String close_output_id)
                    ]
                )
              ]
          coverNode =
            object
              [ ( "cover",
                  object
                    [ ("platform", String "template"),
                      ("name", String (T.pack n)),
                      ( "open_action",
                        Array $ V.fromList [object [("output.turn_on", String open_output_id)]]
                      ),
                      ( "close_action",
                        Array $ V.fromList [object [("output.turn_on", String close_output_id)]]
                      ),
                      ("stop_action", Array $ V.fromList [object [("output.turn_off", String open_output_id)], object [("output.turn_off", String close_output_id)]])
                    ]
                )
              ]
       in openOutputNode : closeOutputNode : coverNode : interpretESP next
    MkButton @name @pin next ->
      let n = symbolVal (Proxy @name)
          p = natVal (Proxy @pin)
          button_id = T.pack n
          binary_sensor_name = T.pack (n <> "_button_press")
          buttonNode =
            object
              [ ( "button",
                  object
                    [ ("platform", String "template"),
                      ("name", String button_id),
                      ("id", String button_id)
                    ]
                )
              ]
          binarySensorNode =
            object
              [ ( "binary_sensor",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String (show p)),
                      ("name", String binary_sensor_name),
                      ("on_press", Array $ V.fromList [object [("button.press", String button_id)]])
                    ]
                )
              ]
       in buttonNode : binarySensorNode : interpretESP next
    MkOutput @name @pin next ->
      let n = symbolVal (Proxy @name)
          p = natVal (Proxy @pin)
          yamlNode =
            object
              [ ( "output",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String (show p)),
                      ("id", String (T.pack n))
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkBinaryOutput @name @pin next ->
      let n = symbolVal (Proxy @name)
          p = natVal (Proxy @pin)
          yamlNode =
            object
              [ ( "switch",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String (show p)),
                      ("id", String (T.pack n)),
                      ("name", String (T.pack n))
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkNumber @name options next ->
      let n = symbolVal (Proxy @name)
          opts =
            [ ("platform", String "template"),
              ("name", String (T.pack n))
            ]
              <> catMaybes
                [ ("min_value",) . String . T.pack . show <$> numberMin options,
                  ("max_value",) . String . T.pack . show <$> numberMax options,
                  ("step",) . String . T.pack . show <$> numberStep options,
                  ("unit_of_measurement",) . String <$> numberUnit options
                ]
          yamlNode =
            object
              [ ( "number",
                  object opts
                )
              ]
       in yamlNode : interpretESP next
    MkSensor @name options next ->
      let n = symbolVal (Proxy @name)
          opts =
            [ ("platform", String "template"),
              ("name", String (T.pack n)),
              ("unit_of_measurement", String (sensorUnit options))
            ]
              <> catMaybes
                [ ("accuracy_decimals",)
                    . String
                    . show
                    <$> sensorAccuracy options,
                  ("update_interval",)
                    . String
                    . (<> "s")
                    . show
                    . (`div` 1000)
                    <$> sensorIntervalMs options
                ]
          yamlNode =
            object
              [ ( "sensor",
                  object opts
                )
              ]
       in yamlNode : interpretESP next
    MkTextSensor @name next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "text_sensor",
                  object
                    [ ("platform", String "template"),
                      ("name", String (T.pack n))
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkBinarySensor @name @pin options next ->
      let n = symbolVal (Proxy @name)
          p = natVal (Proxy @pin)
          yamlNode =
            object
              [ ( "binary_sensor",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String (show p)),
                      ("name", String (T.pack n)),
                      ( "on_press",
                        Array $ V.fromList $ interpretAction $ onPress options
                      )
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkLight @name next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "light",
                  object
                    [ ("platform", String "rgb"),
                      ("name", String (T.pack n))
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkScript @name action next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "script",
                  object
                    [ ("name", String (T.pack n)),
                      ("then", Array $ V.fromList $ interpretAction action)
                    ]
                )
              ]
       in yamlNode : interpretESP next

--------------------------------------------------------------------------------

generateYAML :: IxFree ESPF i j a -> Text
generateYAML prog =
  let nodes = interpretESP prog in yamlToText $ Array $ V.fromList nodes

yamlToText :: Value -> Text
yamlToText = TE.decodeUtf8 . encode
