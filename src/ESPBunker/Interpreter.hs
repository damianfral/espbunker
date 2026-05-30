{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Interpreter where

import Control.Monad.Indexed.Free (IxFree (..))
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Proxy
import Data.Yaml qualified as YAML
import ESPBunker.Actions (interpretAction)
import ESPBunker.Components
import ESPBunker.DSL
import ESPBunker.KeyMapOptions (KeyMapOptions (..))
import ESPBunker.Options
import GHC.TypeLits
import Relude hiding (State, natVal, return)

--------------------------------------------------------------------------------

interpretESP ::
  forall board board' boardName boardNames boardPins.
  (board ~ Board boardName boardNames boardPins, KnownSymbol boardName) =>
  ESPM board board' () -> [Node]
interpretESP (Pure _) = []
interpretESP (Free espf) = case espf of
  MkESPHome @name options next ->
    let n = symbolVal (Proxy @name)
        espHomeOpts = ["name" .= n] <> toKeyMap options
        espHomeNode = NodeObject "esphome" espHomeOpts
     in espHomeNode : interpretESP next
  MkBoard next ->
    let boardName = symbolVal (Proxy @boardName)
        boardSubnode =
          [ "board" .= boardName,
            "framework"
              .= object
                [("type", toJSON FrameworkArduino), ("version", toJSON Latest)]
          ]
        yamlNode = NodeObject "esp32" boardSubnode
     in yamlNode : interpretESP next
  MkLogger next ->
    let espHomeNode = NodeObject "logger" mempty
     in espHomeNode : interpretESP next
  MkSwitch @name @platform @pin opts next ->
    let n = symbolVal (Proxy @name)
        platform = symbolVal (Proxy @(PlatformToSymbol platform))
        yamlNode =
          NodeArray
            "switch"
            [ [ "platform" .= platform,
                "pin" .= pinToText @pin,
                "name" .= n,
                "id" .= snakeCase n
              ]
                <> toKeyMap opts
            ]
     in yamlNode : interpretESP next
  MkCover @name @platform opts next ->
    let n = symbolVal (Proxy @name)
        platform = symbolVal (Proxy @(PlatformToSymbol platform))
        yamlNode =
          NodeArray
            "cover"
            [ ["platform" .= platform, "name" .= n, "id" .= snakeCase n]
                <> toKeyMap opts
            ]
     in yamlNode : interpretESP next
  MkButton @name @pin options next ->
    let n = symbolVal (Proxy @name)
        buttonNode =
          NodeArray
            "button"
            [ fromList
                [("platform", "template"), "name" .= n, "id" .= snakeCase n]
            ]

        binarySensorNode =
          let baseOpts =
                [ "platform" .= String "gpio",
                  "pin" .= pinToText @pin,
                  "name" .= n,
                  "id" .= (snakeCase n <> "_binary_sensor")
                ]
              allOpts = fold @[] [baseOpts, toKeyMap options]
           in NodeArray "binary_sensor" (allOpts :| [])
     in buttonNode : binarySensorNode : interpretESP next
  MkOutput @name @platform @pin opts next ->
    let n = symbolVal (Proxy @name)
        platform = symbolVal (Proxy @(PlatformToSymbol platform))
        yamlNode =
          NodeArray
            "output"
            [ [ "platform" .= platform,
                "id" .= snakeCase n,
                "pin" .= pinToText @pin
              ]
                <> toKeyMap opts
            ]
     in yamlNode : interpretESP next
  MkNumber @name options next ->
    let n = symbolVal (Proxy @name)
        opts =
          [("platform", "template"), "name" .= n, "id" .= snakeCase n]
            <> toKeyMap options
        yamlNode = NodeArray "number" (opts :| [])
     in yamlNode : interpretESP next
  MkSelect @name options next ->
    let n = symbolVal (Proxy @name)
        opts =
          [("platform", "template"), "name" .= n, "id" .= snakeCase n]
            <> toKeyMap options
        yamlNode = NodeArray "select" (opts :| [])
     in yamlNode : interpretESP next
  MkSensor @name @platform @pin options platformOptions next ->
    let n = symbolVal (Proxy @name)
        platform = symbolVal (Proxy @(PlatformToSymbol platform))
        opts =
          [ "platform" .= platform,
            "name" .= n,
            "id" .= snakeCase n,
            "pin" .= natVal (Proxy @pin)
          ]
            <> toKeyMap options
            <> toKeyMap platformOptions
        yamlNode = NodeArray "sensor" (opts :| [])
     in yamlNode : interpretESP next
  MkTextSensor @name next ->
    let n = symbolVal (Proxy @name)
        yamlNode =
          NodeArray
            "text_sensor"
            [ fromList
                [("platform", "template"), "name" .= n, "id" .= snakeCase n]
            ]
     in yamlNode : interpretESP next
  MkBinarySensor @name @platform @pin options platformOptions next ->
    let n = symbolVal (Proxy @name)
        platform = symbolVal (Proxy @(PlatformToSymbol platform))
        baseOpts = ["platform" .= platform, "name" .= n, "id" .= snakeCase n]
        pinModeOpts = case binarySensorPinMode options of
          Nothing -> ["pin" .= pinToText @pin]
          Just mode ->
            let pinObj =
                  [ "number" .= pinToText @pin,
                    "mode"
                      .= object
                        ( catMaybes
                            [ do
                                guard $ pinModeInput mode
                                Just ("input", toJSON True),
                              do
                                guard $ pinModeOutput mode
                                Just ("output", toJSON True),
                              do
                                guard $ pinModeOpenDrain mode
                                Just ("open_drain", toJSON True),
                              do
                                guard $ pinModePullUp mode
                                Just ("pullup", toJSON True),
                              do
                                guard $ pinModePullDown mode
                                Just ("pulldown", toJSON True)
                            ]
                        )
                  ]
             in ["pin" .= object (fromList pinObj)]
        allOpts =
          fold @[]
            [baseOpts, pinModeOpts, toKeyMap options, toKeyMap platformOptions]

        yamlNode = NodeArray "binary_sensor" (allOpts :| [])
     in [yamlNode] <> interpretESP next
  MkLight @name @platform options platformOptions next ->
    let n = symbolVal $ Proxy @name
        platform = symbolVal $ Proxy @(PlatformToSymbol platform)
        yamlNode =
          NodeArray
            "light"
            [ ["platform" .= platform, "name" .= n, "id" .= snakeCase n]
                <> toKeyMap options
                <> toKeyMap platformOptions
            ]
     in [yamlNode] <> interpretESP next
  MkScript @name action next ->
    let n = symbolVal $ Proxy @name
        yamlNode =
          NodeArray
            "script"
            [["id" .= snakeCase n, "then" .= interpretAction action]]
     in yamlNode : interpretESP next
  MkWifi options next ->
    let wifiNode = NodeObject "wifi" (toKeyMap options)
     in wifiNode : interpretESP next
  MkAPI options next ->
    let apiNode = NodeObject "api" (toKeyMap options)
     in apiNode : interpretESP next
  MkOTA options next ->
    case nonEmpty (toKeyMap <$> options) of
      Just otas -> NodeArray "ota" otas : interpretESP next
      Nothing -> interpretESP next
  MkWebServer options next ->
    let webServerNode = NodeObject "web_server" (toKeyMap options)
     in webServerNode : interpretESP next
  MkI2C @name options next ->
    let n = symbolVal (Proxy @name)
        i2cNode = NodeArray "i2c" [["id" .= n] <> toKeyMap options]
     in i2cNode : interpretESP next
  MkPN532I2C @name options next ->
    let n = symbolVal (Proxy @name)
        allFields = ["id" .= snakeCase n] <> toKeyMap options
        yamlNode = NodeArray "pn532_i2c" (allFields :| [])
     in yamlNode : interpretESP next
  MkInterval @name options next ->
    let n = symbolVal (Proxy @name)
        intervalNode =
          NodeArray
            "interval"
            [ ["id" .= maybe (snakeCase n) toString (intervalId options)]
                <> toKeyMap options
            ]
     in intervalNode : interpretESP next

--------------------------------------------------------------------------------

generateYAML ::
  (KnownSymbol boardName) =>
  ESPM (Board boardName boardNames boardPins) board' () -> ByteString
generateYAML prog =
  let nodes = interpretESP prog in YAML.encode $ nodesToKeyMap nodes
