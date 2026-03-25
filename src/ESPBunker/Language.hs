{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Language where

import Control.Monad.Indexed (IxFunctor (imap), ireturn, (>>>=))
import Control.Monad.Indexed.Free (IxFree (..), iliftFree)
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.KeyMap (KeyMap, insert, insertWith)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as Base64
import Data.Default
import Data.Type.Bool (Not)
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import ESPBunker.DeviceClass (DeviceClass)
import GHC.TypeError (Assert)
import GHC.TypeLits
import Relude hiding (State, natVal, return, (>>=))

--------------------------------------------------------------------------------

-- * Platform parameter

data Platform
  = GPIO
  | Out
  | RGB
  | LEDC
  | ESP32_PWM
  | Monochromatic
  | CWWW
  | Endstop
  | ADC
  | Template

data RestoreMode
  = RESTORE_DEFAULT_OFF
  | RESTORE_DEFAULT_ON
  | ALWAYS_OFF
  | ALWAYS_ON
  deriving (Show, Generic)

instance ToJSON RestoreMode where
  toJSON = toJSON @Text . show

type family PlatformToSymbol (platform :: Platform) :: Symbol where
  PlatformToSymbol GPIO = "gpio"
  PlatformToSymbol Out = "output"
  PlatformToSymbol RGB = "rgb"
  PlatformToSymbol LEDC = "ledc"
  PlatformToSymbol ESP32_PWM = "esp32_pwm"
  PlatformToSymbol Monochromatic = "monochromatic"
  PlatformToSymbol CWWW = "cwww"
  PlatformToSymbol Endstop = "endstop"
  PlatformToSymbol ADC = "adc"
  PlatformToSymbol Template = "template"

--------------------------------------------------------------------------------

-- * Components

data ESPHome = ESPHome

newtype ESPHomeOptions = ESPHomeOptions
  { espHomeOnBoot :: Maybe OnBootAction
  }
  deriving (Generic)

data OnBootAction = OnBootAction
  { onBootPriority :: Maybe Int,
    onBootAction :: ESPAction -- The single action to run on boot (can be a sequence)
  }
  deriving (Generic)

instance Default ESPHomeOptions where
  def = ESPHomeOptions Nothing

instance Default OnBootAction where
  def = OnBootAction Nothing (ireturn ())

--------------------------------------------------------------------------------

-- | Binary sensor https://esphome.io/components/binary_sensor/
data BinarySensor (name :: Symbol) (platform :: Platform) (pin :: Nat)
  = BinarySensor

data PinModeType
  = INPUT
  | OUTPUT
  | OPEN_DRAIN
  | PULLUP
  | PULLDOWN
  deriving (Show, Generic)

instance ToJSON PinModeType where
  toJSON INPUT = String "input"
  toJSON OUTPUT = String "output"
  toJSON OPEN_DRAIN = String "open_drain"
  toJSON PULLUP = String "pullup"
  toJSON PULLDOWN = String "pulldown"

data PinMode = PinMode
  { pinModeInput :: Bool,
    pinModeOutput :: Bool,
    pinModeOpenDrain :: Bool,
    pinModePullUp :: Bool,
    pinModePullDown :: Bool
  }
  deriving (Generic)

instance ToJSON PinMode where
  toJSON (PinMode input outp openDrain pullUp pullDown) =
    object
      [ "input" .= input,
        "output" .= outp,
        "open_drain" .= openDrain,
        "pullup" .= pullUp,
        "pulldown" .= pullDown
      ]

data BinarySensorOptions = BinarySensorOptions
  { onPress :: ESPAction,
    onRelease :: ESPAction,
    onClick :: ESPAction,
    onDoubleClick :: ESPAction,
    onLongPress :: ESPAction,
    binarySensorDeviceClass :: Maybe DeviceClass,
    binarySensorIcon :: Maybe Text,
    binarySensorEntityCategory :: Maybe Text,
    binarySensorInternal :: Maybe Bool,
    binarySensorPinMode :: Maybe PinMode -- For GPIO binary sensors
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

noAction :: IxFree ESPActionF i i ()
noAction = ireturn ()

--------------------------------------------------------------------------------

-- | Cover https://esphome.io/components/cover/
data Cover (name :: Symbol) (platform :: Platform) = Cover

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
    { openAction :: ESPAction,
      closeAction :: ESPAction,
      stopAction :: ESPAction,
      openEndstop :: BinarySensor openEndstop openPlatform openPin,
      closeEndstop :: BinarySensor closeEndstop closePlatform closePin,
      openDuration :: Int,
      closeDuration :: Int
    } ->
    CoverEndstopOptions

-- | Light https://esphome.io/components/light/
data Light (name :: Symbol) (platform :: Platform) = Light

data ColorMode
  = COLOR_MODE_UNKNOWN
  | COLOR_MODE_ON_OFF
  | COLOR_MODE_BRIGHTNESS
  | COLOR_MODE_WHITE
  | COLOR_MODE_COLOR_TEMPERATURE
  | COLOR_MODE_COLD_WARM_WHITE
  | COLOR_MODE_RGB
  | COLOR_MODE_RGB_WHITE
  | COLOR_MODE_RGB_COLOR_TEMPERATURE
  | COLOR_MODE_RGB_COLD_WARM_WHITE
  deriving (Show, Generic)

instance ToJSON ColorMode where
  toJSON = toJSON @Text . show

data LightOptions = LightOptions
  { lightTransitionLength :: Maybe Int, -- Mapped to 'transition_length' in ms
    lightEffects :: [Text], -- Mapped to 'effects'
    lightColorMode :: Maybe ColorMode,
    lightGammaCorrect :: Maybe Double,
    lightDefaultTransitionLength :: Maybe Int,
    lightDeviceClass :: Maybe DeviceClass,
    lightIcon :: Maybe Text,
    lightEntityCategory :: Maybe Text,
    lightInternal :: Maybe Bool,
    lightRestoreMode :: Maybe RestoreMode -- Reusing existing RestoreMode
  }

instance Default LightOptions where
  def =
    LightOptions
      Nothing
      []
      Nothing
      Nothing
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
    {ligthMonochromaticOutput :: Output output platform} ->
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

-- | Number https://esphome.io/components/number/
data NumberComponent (name :: Symbol) = NumberComponent

data NumberOptions = NumberOptions
  { numberMin :: Maybe Double,
    numberMax :: Maybe Double,
    numberStep :: Maybe Double,
    numberUnit :: Maybe Text,
    numberDeviceClass :: Maybe DeviceClass,
    numberIcon :: Maybe Text,
    numberEntityCategory :: Maybe Text,
    numberInternal :: Maybe Bool,
    numberMode :: Maybe Text, -- "auto", "box", "slider"
    numberOptimistic :: Maybe Bool -- For template numbers
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

-- | Output https://esphome.io/components/output/
data Output (name :: Symbol) (platform :: Platform) = Output

data OutputGPIOOptions = OutputGPIOOptions
  deriving (Generic)

instance Default OutputGPIOOptions where
  def = OutputGPIOOptions

newtype OutputLEDCOptions = OutputLEDCOptions {frequency :: Maybe Int}
  deriving (Generic)

instance Default OutputLEDCOptions where
  def = OutputLEDCOptions Nothing

-- | Script https://esphome.io/components/script/
data Script (name :: Symbol) = Script

-- | Select https://esphome.io/components/select/
data Select (name :: Symbol) = Select

data SelectOptions = SelectOptions
  { selectOptions :: [Text],
    selectInitial :: Maybe Text,
    selectDeviceClass :: Maybe DeviceClass,
    selectIcon :: Maybe Text,
    selectEntityCategory :: Maybe Text,
    selectInternal :: Maybe Bool,
    selectMode :: Maybe Text -- "auto", "box", "dropdown"
  }

instance Default SelectOptions where
  def = SelectOptions [] Nothing Nothing Nothing Nothing Nothing Nothing

-- | Sensor https://esphome.io/components/sensor/
data Sensor (name :: Symbol) = Sensor

data StateClass
  = STATE_CLASS_MEASUREMENT
  | STATE_CLASS_TOTAL_INCREASING
  | STATE_CLASS_TOTAL
  | STATE_CLASS_NONE
  deriving (Show, Generic)

instance ToJSON StateClass where
  toJSON STATE_CLASS_MEASUREMENT = String "measurement"
  toJSON STATE_CLASS_TOTAL_INCREASING = String "total_increasing"
  toJSON STATE_CLASS_TOTAL = String "total"
  toJSON STATE_CLASS_NONE = String "none"

data SensorOptions = SensorOptions
  { sensorUnit :: Text,
    sensorAccuracy :: Maybe Int,
    sensorIntervalMs :: Maybe Int,
    sensorStateClass :: Maybe StateClass,
    sensorDeviceClass :: Maybe DeviceClass, -- Reusing existing DeviceClass
    sensorIcon :: Maybe Text,
    sensorEntityCategory :: Maybe Text, -- "config", "diagnostic", "system"
    sensorInternal :: Maybe Bool
  }

instance Default SensorOptions where
  def = SensorOptions "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Attenuation
  = ATTEN_0DB
  | ATTEN_2_5DB
  | ATTEN_6DB
  | ATTEN_11DB
  deriving (Show)

instance ToJSON Attenuation where
  toJSON ATTEN_0DB = "0db"
  toJSON ATTEN_2_5DB = "2.5db"
  toJSON ATTEN_6DB = "6db"
  toJSON ATTEN_11DB = "11db"

newtype SensorADCOptions = SensorADCOptions {attenuation :: Maybe Attenuation}

-- | Switch https://esphome.io/components/switch/
data Switch (name :: Symbol) (platform :: Platform) (pin :: Nat) = Switch

data SwitchOptions = SwitchOptions
  { switchRestoreMode :: Maybe RestoreMode, -- Renaming to avoid conflicts
    onTurnOn :: ESPAction,
    onTurnOff :: ESPAction,
    switchDeviceClass :: Maybe DeviceClass,
    switchIcon :: Maybe Text,
    switchEntityCategory :: Maybe Text,
    switchInternal :: Maybe Bool,
    switchOptimistic :: Maybe Bool,
    switchInterlock :: [Text],
    switchInterlockWaitTime :: Maybe Int, -- In milliseconds
    switchInverted :: Maybe Bool -- For GPIO switches
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

data Logger = Logger

-- | I2C Bus https://esphome.io/components/i2c/
data I2C = I2C

data I2COptions = I2COptions
  { i2cSda :: Text, -- Pin for SDA
    i2cScl :: Text, -- Pin for SCL
    i2cScan :: Maybe Bool, -- Whether to scan for I2C devices
    i2cFrequency :: Maybe Text -- I2C bus frequency (e.g. "50kHz")
  }
  deriving (Generic)

instance Default I2COptions where
  def = I2COptions "SDA" "SCL" (Just True) Nothing

-- | PN532 NFC reader I2C component
data PN532I2C = PN532I2C

data PN532I2COptions = PN532I2COptions
  { pn532I2CId :: Maybe Text, -- Component ID
    pn532I2COnTag :: Maybe ESPAction -- Action to run when tag is detected
  }
  deriving (Generic)

instance Default PN532I2COptions where
  def = PN532I2COptions Nothing Nothing

-- | Interval component https://esphome.io/components/interval/
data Interval = Interval

data IntervalOptions = IntervalOptions
  { intervalId :: Maybe Text, -- Component ID (optional)
    intervalInterval :: Maybe Text, -- Interval period with units (e.g. "10s")
    intervalAction :: ESPAction -- Single action to run at the interval (can be a sequence)
  }
  deriving (Generic)

instance Default IntervalOptions where
  def = IntervalOptions Nothing Nothing (ireturn ())

newtype Password = Password {unPassword :: Text}
  deriving newtype (IsString, ToJSON)

data Credentials = Credentials {ssid :: Text, password :: Password}
  deriving (Generic)

instance ToJSON Credentials

newtype WebServerOptions = WebServerOptions {port :: Int}
  deriving (Generic)

instance Default Credentials where
  def = Credentials "" ""

instance Default WebServerOptions where
  def = WebServerOptions 80

data Wifi = Wifi

data WifiOptions = WifiOptions
  { wifiNetworks :: [Credentials],
    wifiAP :: Maybe Credentials
  }
  deriving (Generic)

instance Default WifiOptions where def = WifiOptions [] Nothing

data OTA = OTA

data OTAOptions = OTAOptions {otaPlatform :: Text, otaPassword :: Password}
  deriving (Generic)

data API = API

newtype Base64 = Base64 {unBase64 :: ByteString}
  deriving (Generic)
  deriving newtype (IsString)

newtype APIOptions = APIOptions {apiEncryptionKey :: Base64}
  deriving (Generic)

data WebServer = WebServer

--------------------------------------------------------------------------------

data ESPActionF f g next where
  CloseCover ::
    (KnownSymbol name) =>
    Cover name platform -> next -> ESPActionF f g next
  DecrementNumber ::
    (KnownSymbol name) =>
    NumberComponent name -> Double -> next -> ESPActionF f g next
  Delay :: Int -> next -> ESPActionF f g next
  ComponentUpdate ::
    Text -> next -> ESPActionF f g next -- Updates a component by ID
  ComponentSuspend ::
    Text -> next -> ESPActionF f g next -- Suspends a component by ID
    -- If condition with switch.is_on condition
  IfSwitchIsOn ::
    (KnownSymbol name) =>
    Switch name platform pin -> -- Switch to check
    ESPAction -> -- Then action
    Maybe ESPAction -> -- Else action (optional)
    next ->
    ESPActionF f g next
  IfSwitchIsOff ::
    (KnownSymbol name) =>
    Switch name platform pin -> -- Switch to check
    ESPAction -> -- Then action
    Maybe ESPAction -> -- Else action (optional)
    next ->
    ESPActionF f g next
  IncrementNumber ::
    (KnownSymbol name) =>
    NumberComponent name -> Double -> next -> ESPActionF f g next
  LogMsg :: Text -> next -> ESPActionF f g next
  OpenCover ::
    (KnownSymbol name) =>
    Cover name platform -> next -> ESPActionF f g next
  RunScript ::
    (KnownSymbol name) =>
    Script name -> next -> ESPActionF f g next
  SampleSensor ::
    (KnownSymbol name) =>
    Sensor name -> next -> ESPActionF f g next
  SampleTextSensor ::
    (KnownSymbol name) =>
    Sensor name -> next -> ESPActionF f g next
  SetNumber ::
    (KnownSymbol name) =>
    NumberComponent name -> Double -> next -> ESPActionF f g next
  SetOutputValue ::
    (KnownSymbol name) =>
    Output name platform -> Double -> next -> ESPActionF f g next
  StopCover ::
    (KnownSymbol name) =>
    Cover name platform -> next -> ESPActionF f g next
  ToggleSwitch ::
    (KnownSymbol name) =>
    Switch name platform pin -> next -> ESPActionF f g next
  TurnOffLight ::
    (KnownSymbol name) =>
    Light name platform -> next -> ESPActionF f g next
  TurnOffOutput ::
    (KnownSymbol name) =>
    Output name platform -> next -> ESPActionF f g next
  TurnOffSwitch ::
    (KnownSymbol name) =>
    Switch name platform pin -> next -> ESPActionF f g next
  TurnOnLight ::
    (KnownSymbol name) =>
    Light name platform -> next -> ESPActionF f g next
  TurnOnOutput ::
    (KnownSymbol name) =>
    Output name platform -> next -> ESPActionF f g next
  TurnOnSwitch ::
    (KnownSymbol name) =>
    Switch name platform pin -> next -> ESPActionF f g next

instance IxFunctor ESPActionF where
  imap f (CloseCover n next) = CloseCover n $ f next
  imap f (DecrementNumber n v next) = DecrementNumber n v $ f next
  imap f (Delay ms next) = Delay ms $ f next
  imap f (ComponentUpdate cID next) = ComponentUpdate cID $ f next
  imap f (ComponentSuspend cID next) = ComponentSuspend cID $ f next
  imap f (IfSwitchIsOn sw thenAct elseAct next) = IfSwitchIsOn sw (imap id thenAct) (fmap (imap id) elseAct) $ f next
  imap f (IfSwitchIsOff sw thenAct elseAct next) = IfSwitchIsOff sw (imap id thenAct) (fmap (imap id) elseAct) $ f next
  imap f (IncrementNumber n v next) = IncrementNumber n v $ f next
  imap f (LogMsg msg next) = LogMsg msg $ f next
  imap f (OpenCover n next) = OpenCover n $ f next
  imap f (RunScript sc next) = RunScript sc $ f next
  imap f (SampleSensor n next) = SampleSensor n $ f next
  imap f (SampleTextSensor n next) = SampleTextSensor n $ f next
  imap f (SetNumber n v next) = SetNumber n v $ f next
  imap f (SetOutputValue n v next) = SetOutputValue n v $ f next
  imap f (StopCover n next) = StopCover n $ f next
  imap f (ToggleSwitch n next) = ToggleSwitch n $ f next
  imap f (TurnOffLight l next) = TurnOffLight l $ f next
  imap f (TurnOffOutput n next) = TurnOffOutput n $ f next
  imap f (TurnOffSwitch s next) = TurnOffSwitch s $ f next
  imap f (TurnOnLight l next) = TurnOnLight l $ f next
  imap f (TurnOnOutput n next) = TurnOnOutput n $ f next
  imap f (TurnOnSwitch s next) = TurnOnSwitch s $ f next

newtype IxIdentity i j a = IxIdentity a

instance IxFunctor IxIdentity where
  imap f (IxIdentity a) = IxIdentity $ f a

type ESPAction = IxFree ESPActionF () () ()

--------------------------------------------------------------------------------

turnOn :: (KnownSymbol name) => Switch name platform pin -> ESPAction
turnOn s = iliftFree $ TurnOnSwitch s ()

turnOff :: (KnownSymbol name) => Switch name platform pin -> ESPAction
turnOff s = iliftFree $ TurnOffSwitch s ()

turnOnL :: (KnownSymbol name) => Light name platform -> ESPAction
turnOnL l = iliftFree $ TurnOnLight l ()

turnOffL :: (KnownSymbol name) => Light name platform -> ESPAction
turnOffL l = iliftFree $ TurnOffLight l ()

runScript :: (KnownSymbol name) => Script name -> ESPAction
runScript s = iliftFree $ RunScript s ()

componentUpdate :: Text -> ESPAction
componentUpdate cID = iliftFree $ ComponentUpdate cID ()

componentSuspend :: Text -> ESPAction
componentSuspend cID = iliftFree $ ComponentSuspend cID ()

ifSwitchIsOn :: (KnownSymbol name) => Switch name platform pin -> ESPAction -> Maybe ESPAction -> ESPAction
ifSwitchIsOn sw thenAction elseAction = iliftFree $ IfSwitchIsOn sw thenAction elseAction ()

ifSwitchIsOff :: (KnownSymbol name) => Switch name platform pin -> ESPAction -> Maybe ESPAction -> ESPAction
ifSwitchIsOff sw thenAction elseAction = iliftFree $ IfSwitchIsOff sw thenAction elseAction ()

log :: Text -> ESPAction
log t = iliftFree $ LogMsg t ()

delay :: Int -> ESPAction
delay ms = iliftFree $ Delay ms ()

--------------------------------------------------------------------------------

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

type family If (cond :: Bool) (trueBranch :: k) (falseBranch :: k) :: k where
  If 'True a _ = a
  If 'False _ b = b

-- Insert only if not present
type family Insert (x :: k) (xs :: [k]) :: [k] where
  Insert x xs = If (Elem x xs) xs (x ': xs)

-- Remove first occurrence of x from xs
type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove _ '[] = '[]
  Remove x (x ': xs) = xs
  Remove x (y ': xs) = y ': Remove x xs

-- A constraint to force uniqueness of names (Symbols)
type family AssertNameIsNotUsed (x :: k) (xs :: [k]) :: Constraint where
  AssertNameIsNotUsed x xs =
    Assert
      (Not (Elem x xs))
      (TypeError ('Text "Duplicate id name: " :<>: 'ShowType x))

-- A constraint to force availability of pins (Symbols)
type family AssertPinIsAvailable (x :: k) (xs :: [k]) :: Constraint where
  AssertPinIsAvailable x xs =
    Assert
      (Elem x xs)
      (TypeError ('Text "Pin not available: " :<>: 'ShowType x))

--------------------------------------------------------------------------------

type family PlatformToOptions (component :: k) (platform :: Platform) :: Type where
  PlatformToOptions BinarySensor GPIO = Maybe DeviceClass
  PlatformToOptions Switch GPIO = Maybe DeviceClass
  PlatformToOptions Light RGB = LightRGBOptions
  PlatformToOptions Light Out = LightOutputOptions
  PlatformToOptions Light Monochromatic = LightMonochromaticOptions
  PlatformToOptions Light CWWW = LightCWWWOptions
  PlatformToOptions Output GPIO = OutputGPIOOptions
  PlatformToOptions Output LEDC = OutputLEDCOptions
  PlatformToOptions Cover Endstop = CoverEndstopOptions
  PlatformToOptions Cover Template = CoverOptions
  PlatformToOptions Sensor ADC = SensorADCOptions

--------------------------------------------------------------------------------

-- The combined type-level state that the IxFree will index
-- Define Board as a kind with two type-level lists
data Board (boardName :: Symbol) (names :: [Symbol]) (pins :: [Nat]) = Board

--------------------------------------------------------------------------------

-- * DSL Functor

data ESPF :: Type -> Type -> Type -> Type where
  MkBoard ::
    forall board names pins boardName next.
    (board ~ Board boardName names pins) => next -> ESPF board board next
  MkESPHome ::
    forall name next board.
    (KnownSymbol name) =>
    ESPHomeOptions -> next -> ESPF board board next
  MkLogger :: next -> ESPF board board next
  MkBinarySensor ::
    forall
      name
      platform
      pin
      names
      freePins
      newNames
      newFreePins
      options
      platformSymbol
      board
      boardName
      newBoard
      next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertNameIsNotUsed name names,
      AssertPinIsAvailable pin freePins,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins,
      options ~ PlatformToOptions BinarySensor platform,
      KeyMapOptions options,
      platformSymbol ~ PlatformToSymbol platform,
      KnownSymbol platformSymbol,
      KnownSymbol boardName,
      board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames newFreePins
    ) =>
    BinarySensorOptions -> options -> next -> ESPF board newBoard next
  MkLight ::
    forall
      name
      platform
      names
      freePins
      newNames
      options
      platformSymbol
      boardName
      board
      newBoard
      next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      platformSymbol ~ PlatformToSymbol platform,
      KnownSymbol platformSymbol,
      KeyMapOptions options,
      options ~ PlatformToOptions Light platform,
      KeyMapOptions options,
      board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins
    ) =>
    LightOptions -> options -> next -> ESPF board newBoard next
  MkScript ::
    forall name names freePins newNames boardName board newBoard next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins,
      KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    ESPAction -> next -> ESPF board newBoard next
  MkSwitch ::
    forall name platform pin names freePins newNames newFreePins boardName board newBoard next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins,
      KnownSymbol name,
      KnownNat pin,
      AssertNameIsNotUsed name names,
      AssertPinIsAvailable pin freePins,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins,
      KnownSymbol (PlatformToSymbol platform)
    ) =>
    SwitchOptions -> next -> ESPF board newBoard next
  MkSensor ::
    forall name platform pin names freePins newNames newFreePins boardName board newBoard options next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames newFreePins,
      KnownSymbol name,
      KnownNat pin,
      AssertNameIsNotUsed name names,
      AssertPinIsAvailable pin freePins,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins,
      options ~ PlatformToOptions Sensor platform,
      KnownSymbol (PlatformToSymbol platform),
      KeyMapOptions options
    ) =>
    SensorOptions -> options -> next -> ESPF board newBoard next
  MkTextSensor ::
    forall name names freePins newNames boardName board newBoard next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins,
      KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    next -> ESPF board newBoard next
  MkNumber ::
    forall name names freePins newNames boardName board newBoard next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins,
      KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    NumberOptions -> next -> ESPF board newBoard next
  MkOutput ::
    forall name platform pin names freePins newNames newFreePins boardName options next.
    ( KnownSymbol name,
      KnownSymbol (PlatformToSymbol platform),
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins,
      options ~ PlatformToOptions Output platform,
      KeyMapOptions options
    ) =>
    options ->
    next ->
    ESPF
      (Board boardName names freePins)
      (Board boardName newNames newFreePins)
      next
  MkCover ::
    forall name platform names freePins newNames newFreePins boardName options next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      KnownSymbol (PlatformToSymbol platform),
      options ~ PlatformToOptions Cover platform,
      KeyMapOptions options
    ) =>
    options ->
    next ->
    ESPF
      (Board boardName names freePins)
      (Board boardName newNames newFreePins)
      next
  MkButton ::
    forall name pin names freePins newNames newFreePins boardName next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins
    ) =>
    next ->
    ESPF
      (Board boardName names freePins)
      (Board boardName newNames newFreePins)
      next
  MkWifi :: WifiOptions -> next -> ESPF board board next
  MkAPI :: APIOptions -> next -> ESPF board board next
  MkOTA :: [OTAOptions] -> next -> ESPF board board next
  MkWebServer :: WebServerOptions -> next -> ESPF board board next
  MkI2C :: I2COptions -> next -> ESPF board board next
  MkPN532I2C :: PN532I2COptions -> next -> ESPF board board next
  MkInterval :: IntervalOptions -> next -> ESPF board board next

instance IxFunctor ESPF where
  imap f (MkESPHome @name options next) = MkESPHome @name options $ f next
  imap f (MkLogger next) = MkLogger $ f next
  imap f (MkBinarySensor @name @platform @pin options platformOptions next) =
    MkBinarySensor @name @platform @pin options platformOptions $ f next
  imap f (MkBoard @board next) = MkBoard @board $ f next
  imap f (MkButton @name @pin next) = MkButton @name @pin $ f next
  imap f (MkCover @name @platform opts next) =
    MkCover @name @platform opts $ f next
  imap f (MkLight @name @platform options platformOptions next) =
    MkLight @name @platform options platformOptions $ f next
  imap f (MkNumber @name options next) = MkNumber @name options $ f next
  imap f (MkOutput @name @platform @pin opts next) =
    MkOutput @name @platform @pin opts $ f next
  imap f (MkScript @name options next) = MkScript @name options $ f next
  imap f (MkSensor @name @platform @pin options platformOptions next) =
    MkSensor @name @platform @pin options platformOptions $ f next
  imap f (MkSwitch @name @platform @pin opts next) =
    MkSwitch @name @platform @pin opts $ f next
  imap f (MkTextSensor @name next) = MkTextSensor @name $ f next
  imap f (MkWifi opts next) = MkWifi opts $ f next
  imap f (MkOTA opts next) = MkOTA opts $ f next
  imap f (MkAPI opts next) = MkAPI opts $ f next
  imap f (MkWebServer opts next) = MkWebServer opts $ f next
  imap f (MkI2C opts next) = MkI2C opts $ f next
  imap f (MkPN532I2C opts next) = MkPN532I2C opts $ f next
  imap f (MkInterval opts next) = MkInterval opts $ f next

--------------------------------------------------------------------------------

type ESPM from to a = IxFree ESPF from to a

type ESP32C3 =
  Board
    "esp32-c3-devkitm-1"
    '[]
    '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20, 21]

board ::
  forall board names pins boardName.
  (board ~ Board boardName names pins) =>
  ESPM board board (Board boardName names pins)
board = iliftFree $ MkBoard @board Board

esphome :: forall name board. (KnownSymbol name) => ESPHomeOptions -> ESPM board board ()
esphome options = iliftFree $ MkESPHome @name options ()

logger :: ESPM board board ()
logger = iliftFree $ MkLogger ()

switch ::
  forall name platform pin names freePins newNames newFreePins boardName board newBoard.
  ( board ~ Board boardName names freePins,
    newBoard ~ Board boardName newNames freePins,
    KnownSymbol name,
    KnownNat pin,
    AssertNameIsNotUsed name names,
    AssertPinIsAvailable pin freePins,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins,
    KnownSymbol (PlatformToSymbol platform)
  ) =>
  SwitchOptions ->
  ESPM board newBoard (Switch name platform pin)
switch opts = iliftFree $ MkSwitch @name @platform @pin opts Switch

binarySensor ::
  forall name platform pin names freePins newNames newFreePins options boardName.
  ( KnownSymbol name,
    KnownNat pin,
    AssertNameIsNotUsed name names,
    AssertPinIsAvailable pin freePins,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins,
    options ~ PlatformToOptions BinarySensor platform,
    KnownSymbol (PlatformToSymbol platform),
    KeyMapOptions options,
    KnownSymbol boardName
  ) =>
  BinarySensorOptions ->
  options ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames newFreePins)
    (BinarySensor name platform pin)
binarySensor options platformOptions =
  iliftFree
    $ MkBinarySensor @name @platform @pin options platformOptions BinarySensor

script ::
  forall name names freePins newNames boardName board newBoard.
  ( board ~ Board boardName names freePins,
    newBoard ~ Board boardName newNames freePins,
    KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  ESPAction -> ESPM board newBoard (Script name)
script actions = iliftFree $ MkScript @name actions Script

light ::
  forall name platform names freePins newNames options platformSymbol boardName.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names,
    platformSymbol ~ PlatformToSymbol platform,
    KnownSymbol platformSymbol,
    options ~ PlatformToOptions Light platform,
    KeyMapOptions options
  ) =>
  LightOptions ->
  options ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames freePins)
    (Light name platform)
light options platformOptions =
  iliftFree $ MkLight @name @platform options platformOptions Light

sensor ::
  forall name platform pin names freePins newNames newFreePins boardName board newBoard options.
  ( board ~ Board boardName names freePins,
    newBoard ~ Board boardName newNames newFreePins,
    KnownSymbol name,
    KnownNat pin,
    AssertNameIsNotUsed name names,
    AssertPinIsAvailable pin freePins,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins,
    options ~ PlatformToOptions Sensor platform,
    KeyMapOptions options,
    KnownSymbol (PlatformToSymbol platform)
  ) =>
  SensorOptions -> options -> ESPM board newBoard (Sensor name)
sensor opts platformOpts = iliftFree (MkSensor @name @platform @pin opts platformOpts Sensor)

output ::
  forall name platform pin names freePins newNames newFreePins boardName options.
  ( KnownSymbol name,
    KnownNat pin,
    AssertPinIsAvailable pin freePins,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins,
    KnownSymbol (PlatformToSymbol platform),
    options ~ PlatformToOptions Output platform,
    KeyMapOptions options
  ) =>
  options ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames newFreePins)
    (Output name platform)
output opts = iliftFree (MkOutput @name @platform @pin opts Output)

cover ::
  forall name platform names freePins newNames newFreePins boardName options.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names,
    KnownSymbol (PlatformToSymbol platform),
    options ~ PlatformToOptions Cover platform,
    KeyMapOptions options
  ) =>
  options ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames newFreePins)
    (Cover name platform)
cover opts = iliftFree (MkCover @name @platform opts Cover)

number ::
  forall name names freePins newNames boardName.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  NumberOptions ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames freePins)
    (NumberComponent name)
number opts = iliftFree $ MkNumber @name opts NumberComponent

interval :: IntervalOptions -> ESPM board board Interval
interval opts = iliftFree $ MkInterval opts Interval

wifi :: WifiOptions -> ESPM board board Wifi
wifi opts = iliftFree $ MkWifi opts Wifi

addNetwork :: Text -> Password -> WifiOptions -> WifiOptions
addNetwork ssid p opts =
  opts {wifiNetworks = wifiNetworks opts <> [Credentials ssid p]}

ap :: Text -> Password -> WifiOptions -> WifiOptions
ap ssid p opts = opts {wifiAP = Just $ def {ssid = ssid, password = p}}

-- | Helper to add a boot action to ESPHome options
addBootAction :: Maybe Int -> ESPAction -> ESPHomeOptions -> ESPHomeOptions
addBootAction mbPriority action opts =
  opts {espHomeOnBoot = Just $ OnBootAction mbPriority action}

api :: Base64 -> ESPM board board API
api key = iliftFree $ MkAPI (APIOptions key) API

ota :: [OTAOptions] -> ESPM board board OTA
ota opts = iliftFree $ MkOTA opts OTA

webServer :: Int -> ESPM board board WebServer
webServer port = iliftFree $ MkWebServer (WebServerOptions port) WebServer

i2c :: I2COptions -> ESPM board board I2C
i2c opts = iliftFree $ MkI2C opts I2C

pn532i2c :: PN532I2COptions -> ESPM board board PN532I2C
pn532i2c opts = iliftFree $ MkPN532I2C opts PN532I2C

--------------------------------------------------------------------------------

(>>=) :: ESPM i j a -> (a -> ESPM j k b) -> ESPM i k b
(>>=) = (>>>=) -- the indexed bind from IxFree

--------------------------------------------------------------------------------

done :: ESPM i i ()
done = ireturn ()

--------------------------------------------------------------------------------

-- * Action interpreter

-- Convert ESPAction to YAML automation steps
interpretAction :: IxFree ESPActionF i j () -> Array
interpretAction (Pure _) = empty
interpretAction (Free espf) = case espf of
  ToggleSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "switch.toggle" .= n
     in [option] <> interpretAction next
  TurnOnSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "switch.turn_on" .= n
     in [option] <> interpretAction next
  TurnOffSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "switch.turn_off" .= n
     in [option] <> interpretAction next
  TurnOnLight @name _light next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "light.turn_on" .= n
     in [option] <> interpretAction next
  TurnOffLight @name _light next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "light.turn_off" .= n
     in [option] <> interpretAction next
  LogMsg msg next ->
    let option = Object $ "logger.log" .= msg
     in [option] <> interpretAction next
  Delay ms next ->
    let option = Object $ "delay" .= (show @Text ms <> "ms")
     in [option] <> interpretAction next
  ComponentUpdate cID next ->
    let option = Object $ "component.update" .= cID
     in [option] <> interpretAction next
  ComponentSuspend cID next ->
    let option = Object $ "component.suspend" .= cID
     in [option] <> interpretAction next
  IfSwitchIsOn @name _switch thenAction elseAction next ->
    let n = symbolVal (Proxy @name)
        conditionPart = object ["switch.is_on" .= n]
        thenPart = interpretAction thenAction
        elseMaybe = case elseAction of
          Nothing -> []
          Just ea ->
            ["else" .= interpretAction ea | not $ null (interpretAction ea)]
        ifObject =
          object
            $ [ "condition" .= conditionPart,
                "then" .= thenPart
              ]
            <> elseMaybe
        ifAction = object ["if" .= ifObject]
     in [ifAction] <> interpretAction next
  IfSwitchIsOff @name _switch thenAction elseAction next ->
    let n = symbolVal (Proxy @name)
        conditionPart = object ["switch.is_off" .= n]
        thenPart = interpretAction thenAction
        elseMaybe = case elseAction of
          Nothing -> []
          Just ea ->
            ["else" .= interpretAction ea | not $ null (interpretAction ea)]
        ifObject =
          object
            $ [ "condition" .= conditionPart,
                "then" .= thenPart
              ]
            ++ elseMaybe
        ifAction = object ["if" .= ifObject]
     in [ifAction] <> interpretAction next
  RunScript @name _script next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "script.execute" .= snakeCase n
     in [option] <> interpretAction next
  SetNumber @name _number val next ->
    let n = symbolVal (Proxy @name)
        yamlNode = Object $ "number.set" .= object ["id" .= n, "value" .= val]
     in [yamlNode] <> interpretAction next
  IncrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "number.increment" .= n
     in [option] <> interpretAction next
  DecrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "number.decrement" .= n
     in [option] <> interpretAction next
  SetOutputValue @name _output val next ->
    let n = symbolVal (Proxy @name)
        setLevel = object ["id" .= n, "level" .= val]
        yamlNode = Object $ "output.set_level" .= setLevel
     in [yamlNode] <> interpretAction next
  TurnOnOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "output.turn_on" .= n
     in [option] <> interpretAction next
  TurnOffOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "output.turn_off" .= n
     in [option] <> interpretAction next
  OpenCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "cover.open" .= n
     in [option] <> interpretAction next
  CloseCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "cover.close" .= n
     in [option] <> interpretAction next
  StopCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "cover.stop" .= n
     in [option] <> interpretAction next
  SampleSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "component.update" .= n
     in [option] <> interpretAction next
  SampleTextSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "component.update" .= n
     in [option] <> interpretAction next

--------------------------------------------------------------------------------

data Node = SingleNode Key (KeyMap Value) | Node Key [KeyMap Value]

nodesToKeyMap :: [Node] -> KeyMap Value
nodesToKeyMap = foldl' merge mempty
  where
    merge :: KeyMap Value -> Node -> KeyMap Value
    merge acc (SingleNode name value) = insert name (Object value) acc
    merge acc (Node name values) = insertWith deepMerge name newArray acc
      where
        newArray = toJSON $ Object <$> values

deepMerge :: Value -> Value -> Value
deepMerge (Object km1) (Object km2) = Object $ KM.unionWith deepMerge km1 km2
deepMerge (Array v1) (Array v2) = Array $ v1 <> v2
deepMerge _ v2 = v2

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
                let priorityField = maybe [] (\p -> ["priority" .= p]) mbPriority
                    actionField = ["then" .= interpretAction action]
                 in ["on_boot" .= object (fromList $ priorityField ++ actionField)]
          espHomeOpts = basicOpts <> onBootPart
          espHomeNode = SingleNode "esphome" espHomeOpts
       in espHomeNode : interpretESP next
    MkBoard next ->
      let boardName = symbolVal (Proxy @boardName)
          boardSubnode =
            [ "board" .= boardName,
              "framework" .= object [("type", "arduino"), ("version", "latest")]
            ]
          yamlNode = SingleNode "esp32" boardSubnode
       in yamlNode : interpretESP next
    MkLogger next ->
      let espHomeNode = SingleNode "logger" mempty -- [("level", "DEBUG")]
       in espHomeNode : interpretESP next
    MkSwitch @name @platform @pin opts next ->
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          yamlNode =
            Node
              "switch"
              [ fromList
                  $ [ "platform" .= platform,
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
                      ("optimistic",) . toJSON <$> switchOptimistic opts,
                      ("interlock",) . toJSON <$> if null (switchInterlock opts) then Nothing else Just (switchInterlock opts),
                      switchInterlockWaitTime opts <&> \wait ->
                        ("interlock_wait_time", String $ show wait <> "ms"),
                      switchInverted opts <&> \inv -> ("inverted", toJSON inv)
                    ]
              ]
       in yamlNode : interpretESP next
    MkCover @name @platform opts next ->
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          yamlNode =
            Node
              "cover"
              [ [ "platform" .= platform,
                  "name" .= n,
                  "id" .= snakeCase n
                ]
                  <> toKeyMap opts
              ]
       in yamlNode : interpretESP next
    MkButton @name @pin next ->
      let n = symbolVal (Proxy @name)
          buttonNode =
            Node
              "button"
              [ [ ("platform", "template"),
                  "name" .= n,
                  "id" .= snakeCase n
                ]
              ]

          binarySensorNode =
            Node
              "binary_sensor"
              [ [ ("platform", "gpio"),
                  "pin" .= pinToText @pin,
                  "name" .= n,
                  "id" .= snakeCase n,
                  "on_press" .= V.fromList [object ["button.press" .= n]]
                ]
              ]
       in buttonNode : binarySensorNode : interpretESP next
    MkOutput @name @platform @pin _opts next ->
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          yamlNode =
            Node
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
          yamlNode = Node "number" [opts]
       in yamlNode : interpretESP next
    MkSensor @name @platform @pin options platformOptions next ->
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          opts =
            [ [ "platform" .= platform,
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
                          Just $ "update_interval" .= (show @Text interv <> "ms"),
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
                        sensorInternal options <&> \internal -> "internal" .= internal
                      ]
                  )
                <> toKeyMap platformOptions
            ]
          yamlNode = Node "sensor" opts
       in yamlNode : interpretESP next
    MkTextSensor @name next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            Node
              "text_sensor"
              [[("platform", "template"), "name" .= n, "id" .= snakeCase n]]
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
              Pure () -> [] -- No actions
              Free _ -> ["on_press" .= interpretAction (onPress options)]
          pinModeOpts =
            case binarySensorPinMode options of
              Nothing -> ["pin" .= pinToText @pin] -- Just the pin number
              Just mode ->
                let pinObj =
                      [ "number" .= pinToText @pin,
                        "mode"
                          .= object
                            ( catMaybes
                                [ if pinModeInput mode then Just ("input", toJSON True) else Nothing,
                                  if pinModeOutput mode then Just ("output", toJSON True) else Nothing,
                                  if pinModeOpenDrain mode then Just ("open_drain", toJSON True) else Nothing,
                                  if pinModePullUp mode then Just ("pullup", toJSON True) else Nothing,
                                  if pinModePullDown mode then Just ("pulldown", toJSON True) else Nothing
                                ]
                            )
                      ]
                 in ["pin" .= object (fromList pinObj)]
          allOpts =
            fold @[]
              [baseOpts, actionOpts, pinModeOpts, toKeyMap platformOptions]

          yamlNode = Node "binary_sensor" [allOpts]
       in [yamlNode] <> interpretESP next
    MkLight @name @platform _options platformOptions next ->
      let n = symbolVal $ Proxy @name
          platform = symbolVal $ Proxy @(PlatformToSymbol platform)
          yamlPlatformNode = toKeyMap platformOptions
          yamlNode =
            Node
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
            Node
              "script"
              [ [ "id" .= snakeCase n,
                  "then" .= interpretAction action
                ]
              ]
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
            SingleNode "wifi" $ fromList $ catMaybes [Just networksNode, apNode]
       in wifiNode : interpretESP next
    MkAPI (APIOptions encryptionKey) next ->
      let apiNode =
            SingleNode
              "api"
              [ "encryption"
                  .= object
                    ["key" .= decodeUtf8 @Text (Base64.encode $ unBase64 encryptionKey)]
              ]
       in apiNode : interpretESP next
    MkOTA options next ->
      let otaNode =
            Node "ota" $ options <&> \(OTAOptions platform password) ->
              ["platform" .= platform, "password" .= password]
       in otaNode : interpretESP next
    MkWebServer (WebServerOptions port) next ->
      let webServerNode = SingleNode "web_server" ["port" .= port]
       in webServerNode : interpretESP next
    MkI2C options next ->
      let i2cNode =
            Node
              "i2c"
              [ fromList
                  $ catMaybes
                    [ Just $ "sda" .= i2cSda options,
                      Just $ "scl" .= i2cScl options,
                      i2cScan options <&> \scan -> "scan" .= scan,
                      i2cFrequency options <&> \freq -> "frequency" .= freq
                    ]
              ]
       in i2cNode : interpretESP next
    MkPN532I2C options next ->
      let allFields =
            fromList
              $ catMaybes
                [ pn532I2CId options <&> \cid -> "id" .= cid,
                  pn532I2COnTag options <&> \action -> "on_tag" .= interpretAction action
                ]
          yamlNode = Node "pn532_i2c" [allFields]
       in yamlNode : interpretESP next
    MkInterval options next ->
      let intervalNode =
            Node
              "interval"
              [ [ "id" .= fromMaybe "interval_component" (intervalId options),
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

class KeyMapOptions a where toKeyMap :: a -> KeyMap Value

instance KeyMapOptions DeviceClass where
  toKeyMap deviceClass = KM.singleton "device_class" $ toJSON deviceClass

instance KeyMapOptions BinarySensorOptions where
  toKeyMap BinarySensorOptions {..} =
    [ mapAction "on_press" onPress,
      mapAction "on_release" onRelease,
      mapAction "on_click" onClick,
      mapAction "on_double_click" onDoubleClick,
      mapAction "on_multi_click" onLongPress
    ]
      <> fromList (catMaybes extraOptions)
    where
      mapAction :: Text -> ESPAction -> (Key, Value)
      mapAction key action =
        (fromString $ toString key, Array $ interpretAction action)
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

pinToText :: forall pin. (KnownNat pin) => Text
pinToText = let p = natVal (Proxy @pin) in "GPIO" <> show p
