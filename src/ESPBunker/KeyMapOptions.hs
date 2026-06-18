{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.KeyMapOptions where

import Control.Monad.Free (Free (..))
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Base64 qualified as B64
import Data.Proxy
import ESPBunker.Actions (ESPAction, interpretAction)
import ESPBunker.Components (Credentials (..), EncryptionKey (..))
import ESPBunker.DeviceClass (DeviceClass)
import ESPBunker.Options
import GHC.TypeLits (symbolVal)
import Relude hiding (State, natVal, return)

--------------------------------------------------------------------------------

class KeyMapOptions a where toKeyMap :: a -> KeyMap Value

mapAction :: Text -> ESPAction () -> Maybe (Key, Value)
mapAction key action = case action of
  Pure _ -> Nothing
  Free _ ->
    Just (fromString $ toString key, Array $ interpretAction action)

mapMilliseconds :: Text -> Maybe Int -> Maybe (Key, Value)
mapMilliseconds key val = do
  ms <- val
  Just (fromString $ toString key, String $ show ms <> "ms")

nonEmptyOption :: (ToJSON a) => Text -> [a] -> Maybe (Key, Value)
nonEmptyOption key values = do
  guard $ not $ null values
  let key' = fromString $ toString key
  Just $ key' .= values

instance KeyMapOptions DeviceClass where
  toKeyMap deviceClass = KM.singleton "device_class" $ toJSON deviceClass

instance KeyMapOptions OnBootAction where
  toKeyMap OnBootAction {..} =
    KM.fromList
      $ catMaybes
        [ onBootPriority <&> \priority -> "priority" .= priority,
          Just $ "then" .= interpretAction onBootAction
        ]

instance KeyMapOptions ESPHomeOptions where
  toKeyMap ESPHomeOptions {..} =
    KM.fromList
      $ catMaybes
        [espHomeOnBoot <&> \onBoot -> "on_boot" .= Object (toKeyMap onBoot)]

instance KeyMapOptions BinarySensorOptions where
  toKeyMap BinarySensorOptions {..} =
    fromList (catMaybes actionOptions) <> fromList (catMaybes extraOptions)
    where
      actionOptions =
        [ mapAction "on_press" onPress,
          mapAction "on_release" onRelease,
          mapAction "on_click" onClick,
          mapAction "on_double_click" onDoubleClick,
          mapAction "on_multi_click" onLongPress
        ]
      extraOptions =
        [ binarySensorDeviceClass <&> \dc -> "device_class" .= dc,
          binarySensorIcon <&> \icon -> "icon" .= icon,
          binarySensorEntityCategory <&> \cat -> "entity_category" .= cat,
          binarySensorInternal <&> \internal -> "internal" .= internal
        ]

instance KeyMapOptions LightRGBOptions where
  toKeyMap (LightRGBOptions @red @green @blue _red _green _blue) =
    [ "red" .= symbolVal (Proxy @red),
      "green" .= symbolVal (Proxy @green),
      "blue" .= symbolVal (Proxy @blue)
    ]

instance KeyMapOptions LightOutputOptions where
  toKeyMap (LightOutputOptions @name _) = ["output" .= symbolVal (Proxy @name)]

instance KeyMapOptions LightMonochromaticOptions where
  toKeyMap (LightMonochromaticOptions @output _) =
    ["output" .= symbolVal (Proxy @output)]

instance KeyMapOptions LightCWWWOptions where
  toKeyMap (LightCWWWOptions @coldWhite @warmWhite _ _ cwTemp wwTemp) =
    [ "cold_white" .= symbolVal (Proxy @coldWhite),
      "warm_white" .= symbolVal (Proxy @warmWhite),
      "cold_white_color_temperature" .= cwTemp,
      "warm_white_color_temperature" .= wwTemp
    ]

instance KeyMapOptions CoverEndstopOptions where
  toKeyMap
    ( CoverEndstopOptions
        @openEndstop
        @closeEndstop
        openAction
        closeAction
        stopAction
        _openEndstop
        _closeEndstop
        openDuration
        closeDuration
      ) =
      [ "open_action" .= interpretAction openAction,
        "close_action" .= interpretAction closeAction,
        "stop_action" .= interpretAction stopAction,
        "open_endstop" .= symbolVal (Proxy @openEndstop),
        "close_endstop" .= symbolVal (Proxy @closeEndstop),
        "open_duration" .= String (show openDuration <> "s"),
        "close_duration" .= String (show closeDuration <> "s")
      ]

instance KeyMapOptions CoverOptions where
  toKeyMap CoverOptions {..} =
    fromList
      $ catMaybes
        [ coverDeviceClass <&> \dc -> "device_class" .= dc,
          coverIcon <&> \icon -> "icon" .= icon,
          coverEntityCategory <&> \category -> "entity_category" .= category,
          coverInternal <&> \internal -> "internal" .= internal,
          coverAssumedState <&> \assumed -> "assumed_state" .= assumed,
          coverOptimistic <&> \optimistic -> "optimistic" .= optimistic
        ]

instance KeyMapOptions OutputGPIOOptions where
  toKeyMap _ = mempty

instance (KeyMapOptions a) => KeyMapOptions (Maybe a) where
  toKeyMap Nothing = mempty
  toKeyMap (Just opts) = toKeyMap opts

instance KeyMapOptions OutputLEDCOptions where
  toKeyMap (OutputLEDCOptions frequency) =
    KM.fromList $ catMaybes [("frequency",) . toJSON <$> frequency]

instance KeyMapOptions SensorADCOptions where
  toKeyMap (SensorADCOptions attenuation) =
    KM.fromList $ catMaybes [("attenuation",) . toJSON <$> attenuation]

instance KeyMapOptions NumberOptions where
  toKeyMap NumberOptions {..} =
    KM.fromList
      $ catMaybes
        [ ("min_value",) . toJSON <$> numberMin,
          ("max_value",) . toJSON <$> numberMax,
          ("step",) . toJSON <$> numberStep,
          ("unit_of_measurement",) . toJSON <$> numberUnit,
          numberDeviceClass <&> \dc -> "device_class" .= dc,
          numberIcon <&> \icon -> "icon" .= icon,
          numberEntityCategory <&> \category -> "entity_category" .= category,
          numberInternal <&> \internal -> "internal" .= internal,
          numberMode <&> \mode -> "mode" .= mode,
          numberOptimistic <&> \optimistic -> "optimistic" .= optimistic
        ]

instance KeyMapOptions SelectOptions where
  toKeyMap SelectOptions {..} =
    KM.fromList
      $ catMaybes
        [ nonEmptyOption "options" selectOptions,
          selectInitialOption <&> \initial -> "initial_option" .= initial,
          selectDeviceClass <&> \dc -> "device_class" .= dc,
          selectIcon <&> \icon -> "icon" .= icon,
          selectEntityCategory <&> \category -> "entity_category" .= category,
          selectInternal <&> \internal -> "internal" .= internal,
          selectMode <&> \mode -> "mode" .= mode,
          selectOptimistic <&> \optimistic -> "optimistic" .= optimistic
        ]

instance KeyMapOptions SensorOptions where
  toKeyMap SensorOptions {..} =
    KM.fromList
      $ catMaybes
        [ Just $ "unit_of_measurement" .= sensorUnit,
          sensorAccuracy <&> \accuracy -> "accuracy_decimals" .= accuracy,
          mapMilliseconds "update_interval" sensorIntervalMs,
          sensorStateClass <&> \stateClass -> "state_class" .= stateClass,
          sensorDeviceClass <&> \dc -> "device_class" .= dc,
          sensorIcon <&> \icon -> "icon" .= icon,
          sensorEntityCategory <&> \category -> "entity_category" .= category,
          sensorInternal <&> \internal -> "internal" .= internal
        ]

instance KeyMapOptions SwitchOptions where
  toKeyMap SwitchOptions {..} =
    KM.fromList
      $ catMaybes
        [ ("restore_mode",) . toJSON <$> switchRestoreMode,
          switchDeviceClass <&> \dc -> "device_class" .= dc,
          switchIcon <&> \icon -> "icon" .= icon,
          switchEntityCategory <&> \category -> "entity_category" .= category,
          switchInternal <&> \internal -> "internal" .= internal,
          nonEmptyOption "interlock" switchInterlock,
          mapMilliseconds "interlock_wait_time" switchInterlockWaitTime,
          switchOptimistic <&> \optimistic -> "optimistic" .= optimistic,
          switchInverted <&> \inverted -> "inverted" .= inverted,
          mapAction "on_turn_on" onTurnOn,
          mapAction "on_turn_off" onTurnOff
        ]

instance KeyMapOptions I2COptions where
  toKeyMap I2COptions {..} =
    KM.fromList
      $ catMaybes
        [ Just $ "sda" .= i2cSda,
          Just $ "scl" .= i2cScl,
          i2cScan <&> \scan -> "scan" .= scan,
          i2cFrequency <&> \freq -> "frequency" .= freq
        ]

instance KeyMapOptions PN532I2COptions where
  toKeyMap PN532I2COptions {..} =
    KM.fromList
      $ catMaybes
        [ pn532I2CId <&> \i2cId -> "i2c_id" .= i2cId,
          pn532I2COnTag >>= mapAction "on_tag"
        ]

instance KeyMapOptions IntervalOptions where
  toKeyMap IntervalOptions {..} =
    KM.fromList
      [ "interval" .= fromMaybe "10s" intervalInterval,
        "then" .= interpretAction intervalAction
      ]

instance KeyMapOptions WebServerOptions where
  toKeyMap (WebServerOptions port) = ["port" .= port]

instance KeyMapOptions Credentials where
  toKeyMap Credentials {..} = ["ssid" .= ssid, "password" .= password]

instance KeyMapOptions WifiOptions where
  toKeyMap WifiOptions {..} =
    KM.fromList
      $ catMaybes
        [ nonEmptyOption "networks" (Object . toKeyMap <$> wifiNetworks),
          wifiAP <&> \credentials -> "ap" .= Object (toKeyMap credentials)
        ]

instance KeyMapOptions OTAOptions where
  toKeyMap (OTAOptions platform password) =
    ["platform" .= platform, "password" .= password]

instance KeyMapOptions APIOptions where
  toKeyMap (APIOptions (EncryptionKey key)) =
    ["encryption" .= object ["key" .= decodeUtf8 @Text (B64.encode key)]]

instance KeyMapOptions LightOptions where
  toKeyMap LightOptions {..} =
    KM.fromList
      $ catMaybes
        [ mapEffectsOption "effects" lightEffects,
          mapDoubleOption "gamma_correct" lightGammaCorrect,
          mapMilliseconds "default_transition_length" lightDefaultTransitionLength,
          lightIcon <&> \icon -> "icon" .= icon,
          lightEntityCategory <&> \category -> "entity_category" .= category,
          lightInternal <&> \internal -> "internal" .= internal,
          lightRestoreMode <&> \restore -> "restore_mode" .= restore
        ]
    where
      mapDoubleOption :: Text -> Maybe Double -> Maybe (Key, Value)
      mapDoubleOption key val = do
        doubleVal <- val
        Just (fromString $ toString key, toJSON doubleVal)

      mapEffectsOption :: Text -> [Text] -> Maybe (Key, Value)
      mapEffectsOption key effects = do
        guard $ not $ null effects
        let k' = fromString $ toString key
        Just $ k' .= (toJSON <$> effects)
