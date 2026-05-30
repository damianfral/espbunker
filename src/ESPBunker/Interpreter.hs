{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Interpreter where

import Control.Monad.Indexed.Free (IxFree (..))
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Base64 qualified as B64
import Data.Proxy
import Data.Vector qualified as V
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
interpretESP (Free espf) =
  case espf of
    MkESPHome @name options next ->
      let n = symbolVal (Proxy @name)
          basicOpts = ["name" .= n]
          onBootPart =
            case espHomeOnBoot options of
              Nothing -> []
              Just (OnBootAction mbPriority action) ->
                let priorityField =
                      maybe [] (\p -> ["priority" .= p]) mbPriority
                    actionField = ["then" .= interpretAction action]
                 in ["on_boot" .= object (priorityField <> actionField)]
          espHomeOpts = basicOpts <> onBootPart
          espHomeNode = NodeObject "esphome" espHomeOpts
       in espHomeNode : interpretESP next
    MkBoard next ->
      let boardName = symbolVal (Proxy @boardName)
          boardSubnode =
            [ "board" .= boardName,
              "framework"
                .= object
                  [ ("type", toJSON FrameworkArduino),
                    ("version", toJSON Latest)
                  ]
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
              [ fromList
                  ( [ "platform" .= platform,
                      "pin" .= pinToText @pin,
                      "name" .= n,
                      "id" .= snakeCase n
                    ]
                      <> catMaybes
                        [ ("restore_mode",) . toJSON <$> switchRestoreMode opts,
                          ("device_class",) . toJSON <$> switchDeviceClass opts,
                          ("icon",) . toJSON <$> switchIcon opts,
                          ("entity_category",) . toJSON <$> switchEntityCategory opts,
                          ("internal",) . toJSON <$> switchInternal opts,
                          do
                            let interlockOpts = switchInterlock opts
                            guard $ not $ null interlockOpts
                            Just ("interlock" .= interlockOpts),
                          switchInterlockWaitTime opts <&> \wait ->
                            ("interlock_wait_time", String $ show wait <> "ms"),
                          switchInverted opts <&> \inv -> ("inverted", toJSON inv)
                        ]
                  )
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
                actionOpts =
                  case onPress options of
                    Pure () -> []
                    Free _ -> ["on_press" .= interpretAction (onPress options)]
                allOpts = fold @[] [baseOpts, actionOpts, toKeyMap options]
             in NodeArray "binary_sensor" (allOpts :| [])
       in buttonNode : binarySensorNode : interpretESP next
    MkOutput @name @platform @pin _opts next ->
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          yamlNode =
            NodeArray
              "output"
              [ [ "platform" .= platform,
                  "id" .= snakeCase n,
                  "pin" .= pinToText @pin
                ]
              ]
       in yamlNode : interpretESP next
    MkNumber @name options next ->
      let n = symbolVal (Proxy @name)
          opts :: KeyMap Value =
            fromList
              $ [ ("platform", "template"),
                  "name" .= n,
                  "id" .= snakeCase n
                ]
              <> catMaybes
                [ ("min_value",) . toJSON <$> numberMin options,
                  ("max_value",) . toJSON <$> numberMax options,
                  ("step",) . toJSON <$> numberStep options,
                  ("unit_of_measurement",) . toJSON <$> numberUnit options,
                  ("device_class",) . toJSON <$> numberDeviceClass options,
                  ("icon",) . toJSON <$> numberIcon options,
                  ("entity_category",) . toJSON <$> numberEntityCategory options,
                  ("internal",) . toJSON <$> numberInternal options,
                  ("mode",) . toJSON <$> numberMode options,
                  ("optimistic",) . toJSON <$> numberOptimistic options
                ]
          yamlNode = NodeArray "number" (opts :| [])
       in yamlNode : interpretESP next
    MkSelect @name options next ->
      let n = symbolVal (Proxy @name)
          opts :: KeyMap Value =
            fromList
              $ [ ("platform", "template"),
                  "name" .= n,
                  "id" .= snakeCase n
                ]
              <> catMaybes
                [ do
                    guard $ not $ null $ selectOptions options
                    Just ("options" .= selectOptions options),
                  ("initial_option",) . toJSON <$> selectInitialOption options,
                  ("device_class",) . toJSON <$> selectDeviceClass options,
                  ("icon",) . toJSON <$> selectIcon options,
                  ("entity_category",) . toJSON <$> selectEntityCategory options,
                  ("internal",) . toJSON <$> selectInternal options,
                  ("mode",) . toJSON <$> selectMode options,
                  ("optimistic",) . toJSON <$> selectOptimistic options
                ]
          yamlNode = NodeArray "select" (opts :| [])
       in yamlNode : interpretESP next
    MkSensor @name @platform @pin options platformOptions next ->
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          opts =
            [ "platform" .= platform,
              "name" .= n,
              "id" .= snakeCase n,
              "pin" .= natVal (Proxy @pin),
              "unit_of_measurement" .= sensorUnit options
            ]
              <> fromList
                ( catMaybes
                    [ do
                        accuracy <- sensorAccuracy options
                        Just $ "accuracy_decimals" .= accuracy,
                      do
                        interv <- sensorIntervalMs options
                        Just
                          ("update_interval" .= (show @Text interv <> "ms")),
                      do
                        stateClass <- sensorStateClass options
                        Just $ "state_class" .= stateClass,
                      do
                        deviceClass <- sensorDeviceClass options
                        Just $ "device_class" .= deviceClass,
                      do
                        icon <- sensorIcon options
                        Just $ "icon" .= icon,
                      do
                        entityCategory <- sensorEntityCategory options
                        Just $ "entity_category" .= entityCategory,
                      sensorInternal options
                        <&> \internal -> "internal" .= internal
                    ]
                )
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
          baseOpts =
            [ "platform" .= platform,
              "name" .= n,
              "id" .= snakeCase n
            ]
          actionOpts =
            case onPress options of
              Pure () -> []
              Free _ -> ["on_press" .= interpretAction (onPress options)]
          pinModeOpts =
            case binarySensorPinMode options of
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
              [baseOpts, actionOpts, pinModeOpts, toKeyMap platformOptions]

          yamlNode = NodeArray "binary_sensor" (allOpts :| [])
       in [yamlNode] <> interpretESP next
    MkLight @name @platform _options platformOptions next ->
      let n = symbolVal $ Proxy @name
          platform = symbolVal $ Proxy @(PlatformToSymbol platform)
          yamlPlatformNode = toKeyMap platformOptions
          yamlNode =
            NodeArray
              "light"
              [ [ "platform" .= platform,
                  "name" .= n,
                  "id" .= snakeCase n
                ]
                  <> yamlPlatformNode
              ]
       in [yamlNode] <> interpretESP next
    MkScript @name action next ->
      let n = symbolVal $ Proxy @name
          yamlNode =
            NodeArray
              "script"
              [["id" .= snakeCase n, "then" .= interpretAction action]]
       in yamlNode : interpretESP next
    MkWifi (WifiOptions {..}) next ->
      let networksNode =
            ( "networks",
              Array $ V.fromList $ wifiNetworks <&> \(Credentials {..}) ->
                object ["ssid" .= ssid, "password" .= password]
            )
          apNode =
            wifiAP <&> \Credentials {..} ->
              "ap" .= object ["ssid" .= ssid, "password" .= password]
          wifiNode =
            NodeObject "wifi" (fromList (catMaybes [Just networksNode, apNode]))
       in wifiNode : interpretESP next
    MkAPI (APIOptions encryptionKey) next ->
      let apiNode =
            NodeObject
              "api"
              [ "encryption"
                  .= object
                    [ "key"
                        .= decodeUtf8 @Text
                          (B64.encode $ getEncryptionKey encryptionKey)
                    ]
              ]
       in apiNode : interpretESP next
    MkOTA options next ->
      case nonEmpty
        ( options <&> \(OTAOptions platform password) ->
            fromList ["platform" .= platform, "password" .= password]
        ) of
        Just otas -> NodeArray "ota" otas : interpretESP next
        Nothing -> interpretESP next
    MkWebServer (WebServerOptions port) next ->
      let webServerNode = NodeObject "web_server" (fromList ["port" .= port])
       in webServerNode : interpretESP next
    MkI2C @name options next ->
      let n = symbolVal (Proxy @name)
          i2cNode =
            NodeArray
              "i2c"
              [ KM.fromList
                  $ catMaybes
                    [ Just $ "id" .= n,
                      Just $ "sda" .= i2cSda options,
                      Just $ "scl" .= i2cScl options,
                      i2cScan options <&> \scan -> "scan" .= scan,
                      i2cFrequency options <&> \freq -> "frequency" .= freq
                    ]
              ]
       in i2cNode : interpretESP next
    MkPN532I2C @name options next ->
      let n = symbolVal (Proxy @name)
          allFields =
            fromList
              $ catMaybes
                [ Just $ "id" .= n,
                  pn532I2CId options <&> \cid -> "id" .= cid,
                  pn532I2COnTag options <&> \action ->
                    "on_tag" .= interpretAction action
                ]
          yamlNode = NodeArray "pn532_i2c" (allFields :| [])
       in yamlNode : interpretESP next
    MkInterval @name options next ->
      let n = symbolVal (Proxy @name)
          intervalNode =
            NodeArray
              "interval"
              [ [ "id" .= n,
                  "interval" .= fromMaybe "10s" (intervalInterval options),
                  "then" .= interpretAction (intervalAction options)
                ]
              ]
       in intervalNode : interpretESP next

--------------------------------------------------------------------------------

generateYAML ::
  (KnownSymbol boardName) =>
  ESPM (Board boardName boardNames boardPins) board' () -> ByteString
generateYAML prog =
  let nodes = interpretESP prog in YAML.encode $ nodesToKeyMap nodes
