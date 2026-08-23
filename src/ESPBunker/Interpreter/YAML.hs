{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Interpreter.YAML (generateYAML) where

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Proxy
import Data.Yaml qualified as YAML
import ESPBunker.Actions (interpretAction)
import ESPBunker.Boards ()
import ESPBunker.Components
import ESPBunker.DSL
import ESPBunker.Interpreter.Fold (ifoldFree)
import ESPBunker.KeyMapOptions (KeyMapOptions (..))
import ESPBunker.Options
import GHC.TypeLits (KnownSymbol, symbolVal)
import Relude

--------------------------------------------------------------------------------

interpretESP ::
  forall board board'.
  (KnownSymbol (GetBoardName board)) =>
  ESPM board board' () -> [Node]
interpretESP = execWriter . ifoldFree go
  where
    go :: forall i j. ESPF i j (Writer [Node] ()) -> Writer [Node] ()
    go (MkESPHome @name options next) = do
      let n = symbolVal (Proxy @name)
          espHomeOpts = ["name" .= n] <> toKeyMap options
          espHomeNode = NodeObject "esphome" espHomeOpts
      tell [espHomeNode] >> next
    go (MkBoard next) = do
      let boardName = symbolVal (Proxy @(GetBoardName board))
          boardSubnode =
            [ "board" .= boardName,
              "framework"
                .= object
                  [("type", toJSON FrameworkArduino), ("version", toJSON Latest)]
            ]
          yamlNode = NodeObject "esp32" boardSubnode
      tell [yamlNode] >> next
    go (MkLogger next) = do
      let espHomeNode = NodeObject "logger" mempty
      tell [espHomeNode] >> next
    go (MkSwitch @name @platform @pin opts next) = do
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
      tell [yamlNode] >> next
    go (MkCover @name @platform opts next) = do
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          yamlNode =
            NodeArray
              "cover"
              [ ["platform" .= platform, "name" .= n, "id" .= snakeCase n]
                  <> toKeyMap opts
              ]
      tell [yamlNode] >> next
    go (MkButton @name @pin options next) = do
      let n = symbolVal (Proxy @name)
          buttonNode =
            NodeArray
              "button"
              [[("platform", "template"), "name" .= n, "id" .= snakeCase n]]
          baseOpts =
            [ "platform" .= String "gpio",
              "pin" .= pinToText @pin,
              "name" .= n,
              "id" .= (snakeCase n <> "_binary_sensor")
            ]
          allOpts = fold @[] [baseOpts, toKeyMap options]
          binarySensorNode = NodeArray "binary_sensor" (allOpts :| [])
      tell [buttonNode, binarySensorNode] >> next
    go (MkOutput @name @platform @pin opts next) = do
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
      tell [yamlNode] >> next
    go (MkNumber @name options next) = do
      let n = symbolVal (Proxy @name)
          opts =
            [("platform", "template"), "name" .= n, "id" .= snakeCase n]
              <> toKeyMap options
          yamlNode = NodeArray "number" (opts :| [])
      tell [yamlNode] >> next
    go (MkSelect @name options next) = do
      let n = symbolVal (Proxy @name)
          opts =
            [("platform", "template"), "name" .= n, "id" .= snakeCase n]
              <> toKeyMap options
          yamlNode = NodeArray "select" (opts :| [])
      tell [yamlNode] >> next
    go (MkSensor @name @platform @pin options platformOptions next) = do
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
      tell [yamlNode] >> next
    go (MkTextSensor @name next) = do
      let n = symbolVal (Proxy @name)
          yamlNode =
            NodeArray
              "text_sensor"
              [[("platform", "template"), "name" .= n, "id" .= snakeCase n]]
      tell [yamlNode] >> next
    go (MkBinarySensor @name @platform @pin options platformOptions next) = do
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
               in ["pin" .= object pinObj]
          allOpts =
            mconcat
              [baseOpts, pinModeOpts, toKeyMap options, toKeyMap platformOptions]
          yamlNode = NodeArray "binary_sensor" (allOpts :| [])
      tell [yamlNode] >> next
    go (MkLight @name @platform options platformOptions next) = do
      let n = symbolVal $ Proxy @name
          platform = symbolVal $ Proxy @(PlatformToSymbol platform)
          yamlNode =
            NodeArray
              "light"
              [ ["platform" .= platform, "name" .= n, "id" .= snakeCase n]
                  <> toKeyMap options
                  <> toKeyMap platformOptions
              ]
      tell [yamlNode] >> next
    go (MkScript @name action next) = do
      let n = symbolVal $ Proxy @name
          keyMaps = [["id" .= snakeCase n, "then" .= interpretAction action]]
          yamlNode = NodeArray "script" keyMaps
      tell [yamlNode] >> next
    go (MkWifi options next) = do
      let wifiNode = NodeObject "wifi" (toKeyMap options)
      tell [wifiNode] >> next
    go (MkAPI options next) = do
      let apiNode = NodeObject "api" (toKeyMap options)
      tell [apiNode] >> next
    go (MkOTA options next) = do
      case nonEmpty (toKeyMap <$> options) of
        Just otas -> tell [NodeArray "ota" otas]
        Nothing -> pure ()
      next
    go (MkWebServer options next) = do
      let webServerNode = NodeObject "web_server" (toKeyMap options)
      tell [webServerNode] >> next
    go (MkI2C @name options next) = do
      let n = symbolVal (Proxy @name)
          i2cNode = NodeArray "i2c" [["id" .= n] <> toKeyMap options]
      tell [i2cNode] >> next
    go (MkPN532I2C @name options next) = do
      let n = symbolVal (Proxy @name)
          allFields = ["id" .= snakeCase n] <> toKeyMap options
          yamlNode = NodeArray "pn532_i2c" (allFields :| [])
      tell [yamlNode] >> next
    go (MkInterval @name options next) = do
      let n = symbolVal (Proxy @name)
          keyMaps =
            [ ["id" .= maybe (snakeCase n) toString (intervalId options)]
                <> toKeyMap options
            ]
          intervalNode = NodeArray "interval" keyMaps

      tell [intervalNode] >> next

--------------------------------------------------------------------------------

generateYAML ::
  forall board board'.
  (KnownSymbol (GetBoardName board)) =>
  ESPM board board' () -> ByteString
generateYAML prog =
  let nodes = interpretESP prog in YAML.encode $ nodesToKeyMap nodes
