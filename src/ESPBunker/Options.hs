{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Options where

import Data.Default (Default (..))
import ESPBunker.Actions (ESPAction, noAction)
import ESPBunker.Components
import ESPBunker.DeviceClass (DeviceClass)
import GHC.TypeLits (KnownSymbol)
import Relude hiding (State, natVal, return)

--------------------------------------------------------------------------------

newtype ESPHomeOptions = ESPHomeOptions {espHomeOnBoot :: Maybe OnBootAction}
  deriving (Generic)

instance Default ESPHomeOptions where
  def = ESPHomeOptions Nothing

data OnBootAction = OnBootAction
  { onBootPriority :: Maybe Int,
    onBootAction :: ESPAction ()
  }
  deriving (Generic)

instance Default OnBootAction where
  def = OnBootAction Nothing noAction

--------------------------------------------------------------------------------

data BinarySensorOptions = BinarySensorOptions
  { onPress :: ESPAction (),
    onRelease :: ESPAction (),
    onClick :: ESPAction (),
    onDoubleClick :: ESPAction (),
    onLongPress :: ESPAction (),
    binarySensorDeviceClass :: Maybe DeviceClass,
    binarySensorIcon :: Maybe Text,
    binarySensorEntityCategory :: Maybe Text,
    binarySensorInternal :: Maybe Bool,
    binarySensorPinMode :: Maybe PinMode
  }
  deriving (Generic)

instance Default BinarySensorOptions where
  def =
    BinarySensorOptions
      noAction
      noAction
      noAction
      noAction
      noAction
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

data ButtonOptions = ButtonOptions
  { buttonOnPress :: ESPAction (),
    buttonIcon :: Maybe Text,
    buttonEntityCategory :: Maybe Text,
    buttonInternal :: Maybe Bool
  }
  deriving (Generic)

instance Default ButtonOptions where
  def = ButtonOptions noAction Nothing Nothing Nothing

--------------------------------------------------------------------------------

data CoverOptions = CoverOptions
  { coverDeviceClass :: Maybe DeviceClass,
    coverIcon :: Maybe Text,
    coverEntityCategory :: Maybe Text,
    coverInternal :: Maybe Bool,
    coverAssumedState :: Maybe Bool,
    coverOptimistic :: Maybe Bool
  }

instance Default CoverOptions where
  def = CoverOptions Nothing Nothing Nothing Nothing Nothing Nothing

data CoverEndstopOptions where
  CoverEndstopOptions ::
    forall openEndstop closeEndstop openPlatform openPin closePlatform closePin.
    (KnownSymbol openEndstop, KnownSymbol closeEndstop) =>
    { openAction :: ESPAction (),
      closeAction :: ESPAction (),
      stopAction :: ESPAction (),
      openEndstop :: BinarySensor openEndstop openPlatform openPin,
      closeEndstop :: BinarySensor closeEndstop closePlatform closePin,
      openDuration :: Int,
      closeDuration :: Int
    } ->
    CoverEndstopOptions

--------------------------------------------------------------------------------

data LightOptions = LightOptions
  { lightEffects :: [Text],
    lightGammaCorrect :: Maybe Double,
    lightDefaultTransitionLength :: Maybe Int,
    lightIcon :: Maybe Text,
    lightEntityCategory :: Maybe Text,
    lightInternal :: Maybe Bool,
    lightRestoreMode :: Maybe RestoreMode
  }

instance Default LightOptions where
  def =
    LightOptions
      []
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

data LightRGBOptions where
  LightRGBOptions ::
    forall red green blue platform.
    (KnownSymbol red, KnownSymbol green, KnownSymbol blue) =>
    { red :: Output red platform,
      green :: Output green platform,
      blue :: Output blue platform
    } ->
    LightRGBOptions

data LightOutputOptions where
  LightOutputOptions ::
    forall name. (KnownSymbol name) => Output name GPIO -> LightOutputOptions

data LightMonochromaticOptions where
  LightMonochromaticOptions ::
    forall output platform.
    (KnownSymbol output) =>
    {lightMonochromaticOutput :: Output output platform} ->
    LightMonochromaticOptions

data LightCWWWOptions where
  LightCWWWOptions ::
    forall coldWhite warmWhite platform.
    (KnownSymbol coldWhite, KnownSymbol warmWhite) =>
    { coldWhite :: Output coldWhite platform,
      warmWhite :: Output warmWhite platform,
      coldWhiteColorTemperature :: Text,
      warmWhiteColorTemperature :: Text
    } ->
    LightCWWWOptions

--------------------------------------------------------------------------------

data NumberOptions = NumberOptions
  { numberMin :: Maybe Double,
    numberMax :: Maybe Double,
    numberStep :: Maybe Double,
    numberUnit :: Maybe Text,
    numberDeviceClass :: Maybe DeviceClass,
    numberIcon :: Maybe Text,
    numberEntityCategory :: Maybe Text,
    numberInternal :: Maybe Bool,
    numberMode :: Maybe Text,
    numberOptimistic :: Maybe Bool
  }

instance Default NumberOptions where
  def =
    NumberOptions
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

--------------------------------------------------------------------------------

data OutputGPIOOptions = OutputGPIOOptions
  deriving (Generic)

instance Default OutputGPIOOptions where
  def = OutputGPIOOptions

newtype OutputLEDCOptions = OutputLEDCOptions {frequency :: Maybe Int}
  deriving (Generic)

instance Default OutputLEDCOptions where
  def = OutputLEDCOptions Nothing

--------------------------------------------------------------------------------

data SelectOptions = SelectOptions
  { selectOptions :: [Text],
    selectInitialOption :: Maybe Text,
    selectDeviceClass :: Maybe DeviceClass,
    selectIcon :: Maybe Text,
    selectEntityCategory :: Maybe Text,
    selectInternal :: Maybe Bool,
    selectMode :: Maybe Text,
    selectOptimistic :: Maybe Bool
  }

instance Default SelectOptions where
  def = SelectOptions [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

data SensorOptions = SensorOptions
  { sensorUnit :: Text,
    sensorAccuracy :: Maybe Int,
    sensorIntervalMs :: Maybe Int,
    sensorStateClass :: Maybe StateClass,
    sensorDeviceClass :: Maybe DeviceClass,
    sensorIcon :: Maybe Text,
    sensorEntityCategory :: Maybe Text,
    sensorInternal :: Maybe Bool
  }

instance Default SensorOptions where
  def = SensorOptions "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newtype SensorADCOptions = SensorADCOptions {attenuation :: Maybe Attenuation}

--------------------------------------------------------------------------------

data SwitchOptions = SwitchOptions
  { switchRestoreMode :: Maybe RestoreMode,
    onTurnOn :: ESPAction (),
    onTurnOff :: ESPAction (),
    switchDeviceClass :: Maybe DeviceClass,
    switchIcon :: Maybe Text,
    switchEntityCategory :: Maybe Text,
    switchInternal :: Maybe Bool,
    switchOptimistic :: Maybe Bool,
    switchInterlock :: [Text],
    switchInterlockWaitTime :: Maybe Int,
    switchInverted :: Maybe Bool
  }

instance Default SwitchOptions where
  def =
    SwitchOptions
      Nothing
      noAction
      noAction
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      []
      Nothing
      Nothing

--------------------------------------------------------------------------------

data I2COptions = I2COptions
  { i2cSda :: Text,
    i2cScl :: Text,
    i2cScan :: Maybe Bool,
    i2cFrequency :: Maybe Text
  }
  deriving (Generic)

instance Default I2COptions where
  def = I2COptions "SDA" "SCL" (Just True) Nothing

data PN532I2COptions = PN532I2COptions
  { pn532I2CId :: Maybe Text,
    pn532I2COnTag :: Maybe (ESPAction ())
  }
  deriving (Generic)

instance Default PN532I2COptions where
  def = PN532I2COptions Nothing Nothing

data IntervalOptions = IntervalOptions
  { intervalId :: Maybe Text,
    intervalInterval :: Maybe Text,
    intervalAction :: ESPAction ()
  }
  deriving (Generic)

instance Default IntervalOptions where
  def = IntervalOptions Nothing Nothing noAction

--------------------------------------------------------------------------------

newtype WebServerOptions = WebServerOptions {port :: Int}
  deriving (Generic)

instance Default WebServerOptions where
  def = WebServerOptions 80

data WifiOptions = WifiOptions
  { wifiNetworks :: [Credentials],
    wifiAP :: Maybe Credentials
  }
  deriving (Generic)

instance Default WifiOptions where def = WifiOptions [] Nothing

data OTAOptions = OTAOptions {otaPlatform :: Text, otaPassword :: Password}
  deriving (Generic)

newtype APIOptions = APIOptions {apiEncryptionKey :: EncryptionKey}
  deriving (Generic)

--------------------------------------------------------------------------------

addNetwork :: Text -> Password -> WifiOptions -> WifiOptions
addNetwork ssid p opts =
  opts {wifiNetworks = wifiNetworks opts <> [Credentials ssid p]}

ap :: Text -> Password -> WifiOptions -> WifiOptions
ap ssid p opts = opts {wifiAP = Just $ def {ssid = ssid, password = p}}

addBootAction :: Maybe Int -> ESPAction () -> ESPHomeOptions -> ESPHomeOptions
addBootAction mbPriority action opts =
  opts {espHomeOnBoot = Just $ OnBootAction mbPriority action}
