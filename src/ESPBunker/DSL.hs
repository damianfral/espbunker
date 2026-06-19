{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.DSL where

import Control.Monad.Indexed (IxFunctor (..), ireturn, (>>>=))
import Control.Monad.Indexed.Free (IxFree (..), iliftFree)
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap, insert, insertWith)
import ESPBunker.Actions (ESPAction)
import ESPBunker.Boards ()
import ESPBunker.Components
import ESPBunker.DeviceClass (DeviceClass)
import ESPBunker.KeyMapOptions (KeyMapOptions (..))
import ESPBunker.Options
import GHC.TypeLits
import Relude hiding (State, natVal, return, (>>=))

--------------------------------------------------------------------------------

type PlatformToOptions :: forall k. k -> Platform -> Type
type family PlatformToOptions component platform where
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

data ESPF :: Type -> Type -> Type -> Type where
  MkBoard ::
    forall board names gpioPins adcPins ledcPins boardName next.
    (board ~ Board boardName names gpioPins adcPins ledcPins) =>
    next -> ESPF board board next
  MkESPHome ::
    forall name next board.
    (KnownSymbol name) =>
    ESPHomeOptions -> next -> ESPF board board next
  MkLogger :: next -> ESPF board board next
  MkBinarySensor ::
    forall name platform pin board next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertNameIsAvailable name (GetNames board),
      AssertPinIsAvailable
        PinGPIO
        pin
        (GetGPIOPins board)
        (GetADCPins board)
        (GetLEDCPins board),
      KeyMapOptions (PlatformToOptions BinarySensor platform),
      KnownSymbol (PlatformToSymbol platform)
    ) =>
    BinarySensorOptions ->
    PlatformToOptions BinarySensor platform ->
    next ->
    ESPF board (AddPinComponent name pin board) next
  MkLight ::
    forall name platform board next.
    ( KnownSymbol name,
      AssertNameIsAvailable name (GetNames board),
      KnownSymbol (PlatformToSymbol platform),
      KeyMapOptions (PlatformToOptions Light platform)
    ) =>
    LightOptions ->
    PlatformToOptions Light platform ->
    next ->
    ESPF board (AddComponent name board) next
  MkScript ::
    forall name board next.
    (KnownSymbol name, AssertNameIsAvailable name (GetNames board)) =>
    ESPAction () -> next -> ESPF board (AddComponent name board) next
  MkSwitch ::
    forall name platform pin board next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertNameIsAvailable name (GetNames board),
      AssertPinIsAvailable
        PinGPIO
        pin
        (GetGPIOPins board)
        (GetADCPins board)
        (GetLEDCPins board),
      KnownSymbol (PlatformToSymbol platform)
    ) =>
    SwitchOptions -> next -> ESPF board (AddPinComponent name pin board) next
  MkSensor ::
    forall name platform pin board next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertNameIsAvailable name (GetNames board),
      AssertPinIsAvailable
        PinADC
        pin
        (GetGPIOPins board)
        (GetADCPins board)
        (GetLEDCPins board),
      KnownSymbol (PlatformToSymbol platform),
      KeyMapOptions (PlatformToOptions Sensor platform)
    ) =>
    SensorOptions ->
    PlatformToOptions Sensor platform ->
    next ->
    ESPF board (AddPinComponent name pin board) next
  MkTextSensor ::
    forall name board next.
    ( KnownSymbol name,
      AssertNameIsAvailable name (GetNames board)
    ) =>
    next -> ESPF board (AddComponent name board) next
  MkNumber ::
    forall name board next.
    ( KnownSymbol name,
      AssertNameIsAvailable name (GetNames board)
    ) =>
    NumberOptions -> next -> ESPF board (AddComponent name board) next
  MkSelect ::
    forall name board next.
    ( KnownSymbol name,
      AssertNameIsAvailable name (GetNames board)
    ) =>
    SelectOptions -> next -> ESPF board (AddComponent name board) next
  MkOutput ::
    forall name platform pin board next.
    ( KnownSymbol name,
      KnownSymbol (PlatformToSymbol platform),
      KnownNat pin,
      AssertPinIsAvailable
        (PlatformToPinPlatform platform)
        pin
        (GetGPIOPins board)
        (GetADCPins board)
        (GetLEDCPins board),
      AssertNameIsAvailable name (GetNames board),
      KeyMapOptions (PlatformToOptions Output platform)
    ) =>
    PlatformToOptions Output platform ->
    next ->
    ESPF board (AddPinComponent name pin board) next
  MkCover ::
    forall name platform board next.
    ( KnownSymbol name,
      AssertNameIsAvailable name (GetNames board),
      KnownSymbol (PlatformToSymbol platform),
      KeyMapOptions (PlatformToOptions Cover platform)
    ) =>
    PlatformToOptions Cover platform ->
    next ->
    ESPF board (AddComponent name board) next
  MkButton ::
    forall name pin board next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertPinIsAvailable
        PinGPIO
        pin
        (GetGPIOPins board)
        (GetADCPins board)
        (GetLEDCPins board),
      AssertNameIsAvailable name (GetNames board)
    ) =>
    BinarySensorOptions ->
    next ->
    ESPF board (AddPinComponent name pin board) next
  MkWifi :: WifiOptions -> next -> ESPF board board next
  MkAPI :: APIOptions -> next -> ESPF board board next
  MkOTA :: [OTAOptions] -> next -> ESPF board board next
  MkWebServer :: WebServerOptions -> next -> ESPF board board next
  MkI2C ::
    forall name board next.
    (KnownSymbol name) =>
    I2COptions -> next -> ESPF board board next
  MkPN532I2C ::
    forall name board next.
    (KnownSymbol name) =>
    PN532I2COptions -> next -> ESPF board board next
  MkInterval ::
    forall name board next.
    (KnownSymbol name) =>
    IntervalOptions -> next -> ESPF board board next

instance IxFunctor ESPF where
  imap f (MkESPHome @name options next) = MkESPHome @name options $ f next
  imap f (MkLogger next) = MkLogger $ f next
  imap f (MkBinarySensor @name @platform @pin options platformOptions next) =
    MkBinarySensor @name @platform @pin options platformOptions $ f next
  imap f (MkBoard @board next) = MkBoard @board $ f next
  imap f (MkButton @name @pin options next) =
    MkButton @name @pin options $ f next
  imap f (MkCover @name @platform opts next) =
    MkCover @name @platform opts $ f next
  imap f (MkLight @name @platform options platformOptions next) =
    MkLight @name @platform options platformOptions $ f next
  imap f (MkNumber @name options next) = MkNumber @name options $ f next
  imap f (MkSelect @name opts next) = MkSelect @name opts $ f next
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
  imap f (MkI2C @name opts next) = MkI2C @name opts $ f next
  imap f (MkPN532I2C @name opts next) = MkPN532I2C @name opts $ f next
  imap f (MkInterval @name opts next) = MkInterval @name opts $ f next

--------------------------------------------------------------------------------

type ESPM from to a = IxFree ESPF from to a

--------------------------------------------------------------------------------

board ::
  forall board names gpioPins adcPins ledcPins boardName.
  (board ~ Board boardName names gpioPins adcPins ledcPins) =>
  ESPM board board (Board boardName names gpioPins adcPins ledcPins)
board = iliftFree $ MkBoard @board Board

esphome ::
  forall name board. (KnownSymbol name) => ESPHomeOptions -> ESPM board board ()
esphome options = iliftFree $ MkESPHome @name options ()

logger :: ESPM board board ()
logger = iliftFree $ MkLogger ()

switch ::
  forall name platform pin board.
  ( KnownSymbol name,
    KnownNat pin,
    AssertNameIsAvailable name (GetNames board),
    AssertPinIsAvailable
      PinGPIO
      pin
      (GetGPIOPins board)
      (GetADCPins board)
      (GetLEDCPins board),
    KnownSymbol (PlatformToSymbol platform)
  ) =>
  SwitchOptions ->
  ESPM board (AddPinComponent name pin board) (Switch name platform pin)
switch opts = iliftFree $ MkSwitch @name @platform @pin opts Switch

binarySensor ::
  forall name platform pin board.
  ( KnownSymbol name,
    KnownNat pin,
    AssertNameIsAvailable name (GetNames board),
    AssertPinIsAvailable
      PinGPIO
      pin
      (GetGPIOPins board)
      (GetADCPins board)
      (GetLEDCPins board),
    KnownSymbol (PlatformToSymbol platform),
    KeyMapOptions (PlatformToOptions BinarySensor platform)
  ) =>
  BinarySensorOptions ->
  PlatformToOptions BinarySensor platform ->
  ESPM board (AddPinComponent name pin board) (BinarySensor name platform pin)
binarySensor options platformOptions =
  iliftFree
    $ MkBinarySensor @name @platform @pin options platformOptions BinarySensor

script ::
  forall name board.
  ( KnownSymbol name,
    AssertNameIsAvailable name (GetNames board)
  ) =>
  ESPAction () -> ESPM board (AddComponent name board) (Script name)
script actions = iliftFree $ MkScript @name actions Script

light ::
  forall name platform board.
  ( KnownSymbol name,
    AssertNameIsAvailable name (GetNames board),
    KnownSymbol (PlatformToSymbol platform),
    KeyMapOptions (PlatformToOptions Light platform)
  ) =>
  LightOptions ->
  PlatformToOptions Light platform ->
  ESPM board (AddComponent name board) (Light name platform)
light options platformOptions =
  iliftFree $ MkLight @name @platform options platformOptions Light

sensor ::
  forall name platform pin board.
  ( KnownSymbol name,
    KnownNat pin,
    AssertNameIsAvailable name (GetNames board),
    AssertPinIsAvailable
      PinADC
      pin
      (GetGPIOPins board)
      (GetADCPins board)
      (GetLEDCPins board),
    KeyMapOptions (PlatformToOptions Sensor platform),
    KnownSymbol (PlatformToSymbol platform)
  ) =>
  SensorOptions ->
  PlatformToOptions Sensor platform ->
  ESPM board (AddPinComponent name pin board) (Sensor name)
sensor opts platformOpts =
  iliftFree $ MkSensor @name @platform @pin opts platformOpts Sensor

output ::
  forall name platform pin board.
  ( KnownSymbol name,
    KnownNat pin,
    AssertPinIsAvailable
      (PlatformToPinPlatform platform)
      pin
      (GetGPIOPins board)
      (GetADCPins board)
      (GetLEDCPins board),
    AssertNameIsAvailable name (GetNames board),
    KnownSymbol (PlatformToSymbol platform),
    KeyMapOptions (PlatformToOptions Output platform)
  ) =>
  PlatformToOptions Output platform ->
  ESPM board (AddPinComponent name pin board) (Output name platform)
output opts = iliftFree (MkOutput @name @platform @pin opts Output)

cover ::
  forall name platform board.
  ( KnownSymbol name,
    AssertNameIsAvailable name (GetNames board),
    KnownSymbol (PlatformToSymbol platform),
    KeyMapOptions (PlatformToOptions Cover platform)
  ) =>
  PlatformToOptions Cover platform ->
  ESPM board (AddComponent name board) (Cover name platform)
cover opts = iliftFree (MkCover @name @platform opts Cover)

button ::
  forall name pin board.
  ( KnownSymbol name,
    KnownNat pin,
    AssertPinIsAvailable
      PinGPIO
      pin
      (GetGPIOPins board)
      (GetADCPins board)
      (GetLEDCPins board),
    AssertNameIsAvailable name (GetNames board)
  ) =>
  BinarySensorOptions ->
  ESPM board (AddPinComponent name pin board) ()
button opts = iliftFree (MkButton @name @pin opts ())

number ::
  forall name board.
  ( KnownSymbol name,
    AssertNameIsAvailable name (GetNames board)
  ) =>
  NumberOptions ->
  ESPM board (AddComponent name board) (NumberComponent name)
number opts = iliftFree $ MkNumber @name opts NumberComponent

select ::
  forall name board.
  ( KnownSymbol name,
    AssertNameIsAvailable name (GetNames board)
  ) =>
  SelectOptions ->
  ESPM board (AddComponent name board) (Select name)
select opts = iliftFree $ MkSelect @name opts Select

textSensor ::
  forall name board.
  ( KnownSymbol name,
    AssertNameIsAvailable name (GetNames board)
  ) =>
  ESPM board (AddComponent name board) (TextSensor name)
textSensor = iliftFree $ MkTextSensor @name TextSensor

wifi :: WifiOptions -> ESPM board board Wifi
wifi opts = iliftFree $ MkWifi opts Wifi

api :: EncryptionKey -> ESPM board board API
api key = iliftFree $ MkAPI (APIOptions key) API

ota :: [OTAOptions] -> ESPM board board OTA
ota opts = iliftFree $ MkOTA opts OTA

webServer :: Int -> ESPM board board WebServer
webServer port = iliftFree $ MkWebServer (WebServerOptions port) WebServer

i2c ::
  forall name board.
  (KnownSymbol name) =>
  I2COptions ->
  ESPM board board (I2CBus name)
i2c opts = iliftFree $ MkI2C @name opts I2CBus

pn532i2c ::
  forall name board.
  (KnownSymbol name) =>
  PN532I2COptions ->
  ESPM board board (PN532I2C name)
pn532i2c opts = iliftFree $ MkPN532I2C @name opts PN532I2C

interval ::
  forall name board.
  (KnownSymbol name) =>
  IntervalOptions ->
  ESPM board board (Interval name)
interval opts = iliftFree $ MkInterval @name opts Interval

--------------------------------------------------------------------------------

(>>=) :: ESPM i j a -> (a -> ESPM j k b) -> ESPM i k b
(>>=) = (>>>=)

done :: ESPM i i ()
done = ireturn ()

--------------------------------------------------------------------------------

-- | A single YAML key-value node.
-- 'NodeObject' renders as @key: { ... }@ (a single mapping).
-- 'NodeArray' renders as @key: [{ ... }, ...]@ (an array of mappings).
data Node
  = NodeObject Key (KeyMap Value)
  | NodeArray Key (NonEmpty (KeyMap Value))

nodesToKeyMap :: [Node] -> KeyMap Value
nodesToKeyMap = foldl' merge mempty
  where
    merge :: KeyMap Value -> Node -> KeyMap Value
    merge acc (NodeObject name value) = insert name (Object value) acc
    merge acc (NodeArray name values) =
      insertWith deepMerge name (toJSON $ Object <$> values) acc
