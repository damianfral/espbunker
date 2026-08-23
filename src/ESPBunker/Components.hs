{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Components where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Default (Default (..))
import Data.Type.Bool (Not)
import GHC.TypeError (Assert)
import GHC.TypeLits
import Relude hiding (State, natVal)

--------------------------------------------------------------------------------

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

data PinPlatform = PinGPIO | PinADC | PinLEDC

data RestoreMode
  = RESTORE_DEFAULT_OFF
  | RESTORE_DEFAULT_ON
  | ALWAYS_OFF
  | ALWAYS_ON
  deriving (Show, Generic)

instance ToJSON RestoreMode where
  toJSON = toJSON @Text . show

data FrameworkType = FrameworkArduino | FrameworkESPHome
  deriving (Show, Generic)

instance ToJSON FrameworkType where
  toJSON FrameworkArduino = String "arduino"
  toJSON FrameworkESPHome = String "esphome"

data FrameworkVersion = Latest | Version Text
  deriving (Show, Generic)

instance ToJSON FrameworkVersion where
  toJSON Latest = String "latest"
  toJSON (Version v) = String v

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

type family OutputPinPlatform (p :: Platform) :: PinPlatform where
  OutputPinPlatform GPIO = PinGPIO
  OutputPinPlatform LEDC = PinLEDC

--------------------------------------------------------------------------------

data ESPHome = ESPHome

data BinarySensor (name :: Symbol) (platform :: Platform) (pin :: Nat)
  = BinarySensor

data Cover (name :: Symbol) (platform :: Platform) = Cover

data Light (name :: Symbol) (platform :: Platform) = Light

data NumberComponent (name :: Symbol) = NumberComponent

data Output (name :: Symbol) (platform :: Platform) = Output

data Script (name :: Symbol) = Script

data Select (name :: Symbol) = Select

data Sensor (name :: Symbol) = Sensor

data TextSensor (name :: Symbol) = TextSensor

data Switch (name :: Symbol) (platform :: Platform) (pin :: Nat) = Switch

data Logger = Logger

data I2CBus (name :: Symbol) = I2CBus

data PN532I2C (name :: Symbol) = PN532I2C

data Interval (name :: Symbol) = Interval

data Wifi = Wifi

data OTA = OTA

data API = API

data WebServer = WebServer

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

newtype Password = Password {unPassword :: Text}
  deriving newtype (IsString, ToJSON)

data Credentials = Credentials {ssid :: Text, password :: Password}
  deriving (Generic)

instance ToJSON Credentials

instance Default Credentials where
  def = Credentials "" ""

-- | Raw key material for the ESPHome API encryption (AES-256).
-- Provide exactly 32 ASCII characters (32 bytes); the interpreter
-- base64-encodes this to a 44-character key in the generated YAML.
newtype EncryptionKey = EncryptionKey {getEncryptionKey :: ByteString}
  deriving (Generic)
  deriving newtype (IsString)

--------------------------------------------------------------------------------

data
  Board
    (boardName :: Symbol)
    (names :: [Symbol])
    (gpioPins :: [Nat])
    (adcPins :: [Nat])
    (ledcPins :: [Nat])
  = Board

--------------------------------------------------------------------------------

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

type family If (cond :: Bool) (trueBranch :: k) (falseBranch :: k) :: k where
  If 'True a _ = a
  If 'False _ b = b

type family Insert (x :: k) (xs :: [k]) :: [k] where
  Insert x xs = If (Elem x xs) xs (x ': xs)

type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove _ '[] = '[]
  Remove x (x ': xs) = xs
  Remove x (y ': xs) = y ': Remove x xs

type family AssertNameIsAvailable (x :: k) (xs :: [k]) :: Constraint where
  AssertNameIsAvailable x xs =
    Assert
      (Not (Elem x xs))
      (TypeError ('Text "Name not available: " :<>: 'ShowType x))

type family
  AssertPinIsAvailable
    (pp :: PinPlatform)
    (pin :: Nat)
    (gpioPins :: [Nat])
    (adcPins :: [Nat])
    (ledcPins :: [Nat]) ::
    Constraint
  where
  AssertPinIsAvailable PinGPIO pin gpioPins _ _ =
    Assert
      (Elem pin gpioPins)
      (TypeError ('Text "GPIO pin not available: " :<>: 'ShowType pin))
  AssertPinIsAvailable PinADC pin _ adcPins _ =
    Assert
      (Elem pin adcPins)
      (TypeError ('Text "ADC pin not available: " :<>: 'ShowType pin))
  AssertPinIsAvailable PinLEDC pin _ _ ledcPins =
    Assert
      (Elem pin ledcPins)
      (TypeError ('Text "LEDC pin not available: " :<>: 'ShowType pin))

type family GetNames (board :: Type) :: [Symbol] where
  GetNames (Board _ names _ _ _) = names

type family GetGPIOPins (board :: Type) :: [Nat] where
  GetGPIOPins (Board _ _ gpioPins _ _) = gpioPins

type family GetADCPins (board :: Type) :: [Nat] where
  GetADCPins (Board _ _ _ adcPins _) = adcPins

type family GetLEDCPins (board :: Type) :: [Nat] where
  GetLEDCPins (Board _ _ _ _ ledcPins) = ledcPins

type family GetBoardName (board :: Type) :: Symbol where
  GetBoardName (Board name _ _ _ _) = name

type family AddPinComponent (name :: Symbol) (pin :: Nat) (board :: Type) :: Type where
  AddPinComponent name pin (Board boardName names gpioPins adcPins ledcPins) =
    Board
      boardName
      (Insert name names)
      (Remove pin gpioPins)
      (Remove pin adcPins)
      (Remove pin ledcPins)

type family AddComponent (name :: Symbol) (board :: Type) :: Type where
  AddComponent name (Board boardName names gpioPins adcPins ledcPins) =
    Board boardName (Insert name names) gpioPins adcPins ledcPins

--------------------------------------------------------------------------------

pinToText :: forall pin. (KnownNat pin) => Text
pinToText = let p = natVal (Proxy @pin) in "GPIO" <> show p

deepMerge :: Value -> Value -> Value
deepMerge (Object km1) (Object km2) = Object $ KM.unionWith deepMerge km1 km2
deepMerge (Array v1) (Array v2) = Array $ v1 <> v2
deepMerge _ v2 = v2
