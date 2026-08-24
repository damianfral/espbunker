{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeAbstractions #-}
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
import Relude

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

-- | Omit the key when the value is 'Nothing'; otherwise emit @key: value@.
(.=?) :: (ToJSON a) => Text -> Maybe a -> Maybe (Key, Value)
(.=?) key = fmap (fromString (toString key) .=)

actionField :: Text -> ESPAction () -> Maybe (Key, Value)
actionField = mapAction

msField :: Text -> Maybe Int -> Maybe (Key, Value)
msField = mapMilliseconds

nonEmptyField :: (ToJSON a) => Text -> [a] -> Maybe (Key, Value)
nonEmptyField = nonEmptyOption

instance KeyMapOptions DeviceClass where
  toKeyMap deviceClass = KM.singleton "device_class" $ toJSON deviceClass

instance KeyMapOptions OnBootAction where
  toKeyMap OnBootAction {..} =
    KM.fromList
      $ catMaybes
        [ "priority" .=? onBootPriority,
          Just $ "then" .= interpretAction onBootAction
        ]

instance KeyMapOptions ESPHomeOptions where
  toKeyMap ESPHomeOptions {..} =
    KM.fromList
      $ catMaybes ["on_boot" .=? (Object . toKeyMap <$> espHomeOnBoot)]

instance KeyMapOptions BinarySensorOptions where
  toKeyMap BinarySensorOptions {..} =
    fromList (catMaybes actionOptions) <> fromList (catMaybes extraOptions)
    where
      actionOptions =
        [ actionField "on_press" onPress,
          actionField "on_release" onRelease,
          actionField "on_click" onClick,
          actionField "on_double_click" onDoubleClick,
          actionField "on_multi_click" onLongPress
        ]
      extraOptions =
        [ "icon" .=? binarySensorIcon,
          "entity_category" .=? binarySensorEntityCategory,
          "internal" .=? binarySensorInternal
        ]

instance KeyMapOptions ButtonOptions where
  toKeyMap ButtonOptions {..} =
    fromList
      $ catMaybes
        [ actionField "on_press" buttonOnPress,
          "icon" .=? buttonIcon,
          "entity_category" .=? buttonEntityCategory,
          "internal" .=? buttonInternal
        ]

instance KeyMapOptions LightRGBOptions where
  toKeyMap (LightRGBOptions @red @green @blue _red _green _blue) =
    [ "red" .= symbolVal (Proxy @red),
      "green" .= symbolVal (Proxy @green),
      "blue" .= symbolVal (Proxy @blue)
    ]

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
        endstopCoverOptions
      ) =
      toKeyMap endstopCoverOptions
        <> [ "open_action" .= interpretAction openAction,
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
        [ "device_class" .=? coverDeviceClass,
          "icon" .=? coverIcon,
          "entity_category" .=? coverEntityCategory,
          "internal" .=? coverInternal,
          "assumed_state" .=? coverAssumedState,
          "optimistic" .=? coverOptimistic
        ]

instance KeyMapOptions OutputGPIOOptions where
  toKeyMap _ = mempty

instance (KeyMapOptions a) => KeyMapOptions (Maybe a) where
  toKeyMap Nothing = mempty
  toKeyMap (Just opts) = toKeyMap opts

instance KeyMapOptions OutputLEDCOptions where
  toKeyMap (OutputLEDCOptions frequency) =
    KM.fromList $ catMaybes ["frequency" .=? frequency]

instance KeyMapOptions SensorADCOptions where
  toKeyMap (SensorADCOptions attenuation) =
    KM.fromList $ catMaybes ["attenuation" .=? attenuation]

instance KeyMapOptions NumberOptions where
  toKeyMap NumberOptions {..} =
    KM.fromList
      $ catMaybes
        [ "min_value" .=? numberMin,
          "max_value" .=? numberMax,
          "step" .=? numberStep,
          "unit_of_measurement" .=? numberUnit,
          "device_class" .=? numberDeviceClass,
          "icon" .=? numberIcon,
          "entity_category" .=? numberEntityCategory,
          "internal" .=? numberInternal,
          "mode" .=? numberMode,
          "optimistic" .=? numberOptimistic
        ]

instance KeyMapOptions SelectOptions where
  toKeyMap SelectOptions {..} =
    KM.fromList
      $ catMaybes
        [ nonEmptyField "options" selectOptions,
          "initial_option" .=? selectInitialOption,
          "device_class" .=? selectDeviceClass,
          "icon" .=? selectIcon,
          "entity_category" .=? selectEntityCategory,
          "internal" .=? selectInternal,
          "mode" .=? selectMode,
          "optimistic" .=? selectOptimistic
        ]

instance KeyMapOptions SensorOptions where
  toKeyMap SensorOptions {..} =
    KM.fromList
      $ catMaybes
        [ Just $ "unit_of_measurement" .= sensorUnit,
          "accuracy_decimals" .=? sensorAccuracy,
          msField "update_interval" sensorIntervalMs,
          "state_class" .=? sensorStateClass,
          "device_class" .=? sensorDeviceClass,
          "icon" .=? sensorIcon,
          "entity_category" .=? sensorEntityCategory,
          "internal" .=? sensorInternal
        ]

instance KeyMapOptions SwitchOptions where
  toKeyMap SwitchOptions {..} =
    KM.fromList
      $ catMaybes
        [ "restore_mode" .=? switchRestoreMode,
          "device_class" .=? switchDeviceClass,
          "icon" .=? switchIcon,
          "entity_category" .=? switchEntityCategory,
          "internal" .=? switchInternal,
          nonEmptyField "interlock" switchInterlock,
          msField "interlock_wait_time" switchInterlockWaitTime,
          "optimistic" .=? switchOptimistic,
          "inverted" .=? switchInverted,
          actionField "on_turn_on" onTurnOn,
          actionField "on_turn_off" onTurnOff
        ]

instance KeyMapOptions I2COptions where
  toKeyMap I2COptions {..} =
    KM.fromList
      $ catMaybes
        [ Just $ "sda" .= i2cSda,
          Just $ "scl" .= i2cScl,
          "scan" .=? i2cScan,
          "frequency" .=? i2cFrequency
        ]

instance KeyMapOptions PN532I2COptions where
  toKeyMap PN532I2COptions {..} =
    KM.fromList
      $ catMaybes
        [ "i2c_id" .=? pn532I2CId,
          pn532I2COnTag >>= actionField "on_tag"
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
        [ nonEmptyField "networks" (Object . toKeyMap <$> wifiNetworks),
          "ap" .=? (Object . toKeyMap <$> wifiAP)
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
          msField "default_transition_length" lightDefaultTransitionLength,
          "icon" .=? lightIcon,
          "entity_category" .=? lightEntityCategory,
          "internal" .=? lightInternal,
          "restore_mode" .=? lightRestoreMode
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
