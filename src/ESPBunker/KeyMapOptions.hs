{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.KeyMapOptions where

import Control.Monad.Indexed.Free (IxFree (..))
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Proxy
import ESPBunker.Actions (ESPAction, interpretAction)
import ESPBunker.Components (ColorMode)
import ESPBunker.DeviceClass (DeviceClass)
import ESPBunker.Options
import GHC.TypeLits (symbolVal)
import Relude hiding (State, natVal, return)

--------------------------------------------------------------------------------

class KeyMapOptions a where toKeyMap :: a -> KeyMap Value

instance KeyMapOptions DeviceClass where
  toKeyMap deviceClass = KM.singleton "device_class" $ toJSON deviceClass

instance KeyMapOptions BinarySensorOptions where
  toKeyMap BinarySensorOptions {..} =
    fromList (catMaybes actionOptions) <> fromList (catMaybes extraOptions)
    where
      mapAction :: Text -> ESPAction -> Maybe (Key, Value)
      mapAction key action = case action of
        Pure _ -> Nothing
        Free _ ->
          Just (fromString $ toString key, Array $ interpretAction action)
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

instance KeyMapOptions LightOptions where
  toKeyMap LightOptions {..} =
    KM.fromList
      $ catMaybes
        [ mapIntOption "transition_length" lightTransitionLength,
          mapEffectsOption "effects" lightEffects,
          mapColorMode "color_mode" lightColorMode,
          mapDoubleOption "gamma_correct" lightGammaCorrect,
          mapIntOption "default_transition_length" lightDefaultTransitionLength,
          lightDeviceClass <&> \dc -> "device_class" .= dc,
          lightIcon <&> \icon -> "icon" .= icon,
          lightEntityCategory <&> \category -> "entity_category" .= category,
          lightInternal <&> \internal -> "internal" .= internal,
          lightRestoreMode <&> \restore -> "restore_mode" .= restore
        ]
    where
      mapIntOption :: Text -> Maybe Int -> Maybe (Key, Value)
      mapIntOption key val = do
        transitionL <- val
        Just (fromString $ toString key, String $ show transitionL <> "ms")

      mapDoubleOption :: Text -> Maybe Double -> Maybe (Key, Value)
      mapDoubleOption key val = do
        doubleVal <- val
        Just (fromString $ toString key, toJSON doubleVal)

      mapColorMode :: Text -> Maybe ColorMode -> Maybe (Key, Value)
      mapColorMode key mode = do
        colorMode <- mode
        Just (fromString $ toString key, toJSON colorMode)

      mapEffectsOption :: Text -> [Text] -> Maybe (Key, Value)
      mapEffectsOption key effects = do
        guard $ not $ null effects
        let k' = fromString $ toString key
        Just $ k' .= (toJSON <$> effects)
