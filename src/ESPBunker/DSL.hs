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
      AssertNameIsAvailable name names,
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
      AssertNameIsAvailable name names,
      newNames ~ Insert name names,
      platformSymbol ~ PlatformToSymbol platform,
      KnownSymbol platformSymbol,
      KeyMapOptions options,
      options ~ PlatformToOptions Light platform,
      board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins
    ) =>
    LightOptions -> options -> next -> ESPF board newBoard next
  MkScript ::
    forall name names freePins newNames boardName board newBoard next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins,
      KnownSymbol name,
      AssertNameIsAvailable name names,
      newNames ~ Insert name names
    ) =>
    ESPAction -> next -> ESPF board newBoard next
  MkSwitch ::
    forall
      name
      platform
      pin
      names
      freePins
      newNames
      newFreePins
      boardName
      board
      newBoard
      next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames newFreePins,
      KnownSymbol name,
      KnownNat pin,
      AssertNameIsAvailable name names,
      AssertPinIsAvailable pin freePins,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins,
      KnownSymbol (PlatformToSymbol platform)
    ) =>
    SwitchOptions -> next -> ESPF board newBoard next
  MkSensor ::
    forall
      name
      platform
      pin
      names
      freePins
      newNames
      newFreePins
      boardName
      board
      newBoard
      options
      next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames newFreePins,
      KnownSymbol name,
      KnownNat pin,
      AssertNameIsAvailable name names,
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
      AssertNameIsAvailable name names,
      newNames ~ Insert name names
    ) =>
    next -> ESPF board newBoard next
  MkNumber ::
    forall name names freePins newNames boardName board newBoard next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins,
      KnownSymbol name,
      AssertNameIsAvailable name names,
      newNames ~ Insert name names
    ) =>
    NumberOptions -> next -> ESPF board newBoard next
  MkSelect ::
    forall name names freePins newNames boardName board newBoard next.
    ( board ~ Board boardName names freePins,
      newBoard ~ Board boardName newNames freePins,
      KnownSymbol name,
      AssertNameIsAvailable name names,
      newNames ~ Insert name names
    ) =>
    SelectOptions -> next -> ESPF board newBoard next
  MkOutput ::
    forall
      name
      platform
      pin
      names
      freePins
      newNames
      newFreePins
      boardName
      options
      next.
    ( KnownSymbol name,
      KnownSymbol (PlatformToSymbol platform),
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsAvailable name names,
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
    forall name platform names freePins newNames boardName options next.
    ( KnownSymbol name,
      AssertNameIsAvailable name names,
      newNames ~ Insert name names,
      KnownSymbol (PlatformToSymbol platform),
      options ~ PlatformToOptions Cover platform,
      KeyMapOptions options
    ) =>
    options ->
    next ->
    ESPF
      (Board boardName names freePins)
      (Board boardName newNames freePins)
      next
  MkButton ::
    forall name pin names freePins newNames newFreePins boardName next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsAvailable name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins
    ) =>
    BinarySensorOptions ->
    next ->
    ESPF
      (Board boardName names freePins)
      (Board boardName newNames newFreePins)
      next
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

type ESP32C3 =
  Board
    "esp32-c3-devkitm-1"
    '[]
    '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20, 21]

--------------------------------------------------------------------------------

board ::
  forall board names pins boardName.
  (board ~ Board boardName names pins) =>
  ESPM board board (Board boardName names pins)
board = iliftFree $ MkBoard @board Board

esphome ::
  forall name board. (KnownSymbol name) => ESPHomeOptions -> ESPM board board ()
esphome options = iliftFree $ MkESPHome @name options ()

logger :: ESPM board board ()
logger = iliftFree $ MkLogger ()

switch ::
  forall
    name
    platform
    pin
    names
    freePins
    newNames
    newFreePins
    boardName
    board
    newBoard.
  ( board ~ Board boardName names freePins,
    newBoard ~ Board boardName newNames newFreePins,
    KnownSymbol name,
    KnownNat pin,
    AssertNameIsAvailable name names,
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
    AssertNameIsAvailable name names,
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
    AssertNameIsAvailable name names,
    newNames ~ Insert name names
  ) =>
  ESPAction -> ESPM board newBoard (Script name)
script actions = iliftFree $ MkScript @name actions Script

light ::
  forall name platform names freePins newNames options platformSymbol boardName.
  ( KnownSymbol name,
    AssertNameIsAvailable name names,
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
  forall
    name
    platform
    pin
    names
    freePins
    newNames
    newFreePins
    boardName
    board
    newBoard
    options.
  ( board ~ Board boardName names freePins,
    newBoard ~ Board boardName newNames newFreePins,
    KnownSymbol name,
    KnownNat pin,
    AssertNameIsAvailable name names,
    AssertPinIsAvailable pin freePins,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins,
    options ~ PlatformToOptions Sensor platform,
    KeyMapOptions options,
    KnownSymbol (PlatformToSymbol platform)
  ) =>
  SensorOptions -> options -> ESPM board newBoard (Sensor name)
sensor opts platformOpts =
  iliftFree $ MkSensor @name @platform @pin opts platformOpts Sensor

output ::
  forall name platform pin names freePins newNames newFreePins boardName options.
  ( KnownSymbol name,
    KnownNat pin,
    AssertPinIsAvailable pin freePins,
    AssertNameIsAvailable name names,
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
  forall name platform names freePins newNames boardName options.
  ( KnownSymbol name,
    AssertNameIsAvailable name names,
    newNames ~ Insert name names,
    KnownSymbol (PlatformToSymbol platform),
    options ~ PlatformToOptions Cover platform,
    KeyMapOptions options
  ) =>
  options ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames freePins)
    (Cover name platform)
cover opts = iliftFree (MkCover @name @platform opts Cover)

button ::
  forall name pin names freePins newNames newFreePins boardName.
  ( KnownSymbol name,
    KnownNat pin,
    AssertPinIsAvailable pin freePins,
    AssertNameIsAvailable name names,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins
  ) =>
  BinarySensorOptions ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames newFreePins)
    ()
button opts = iliftFree (MkButton @name @pin opts ())

number ::
  forall name names freePins newNames boardName.
  ( KnownSymbol name,
    AssertNameIsAvailable name names,
    newNames ~ Insert name names
  ) =>
  NumberOptions ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames freePins)
    (NumberComponent name)
number opts = iliftFree $ MkNumber @name opts NumberComponent

select ::
  forall name names freePins newNames boardName.
  ( KnownSymbol name,
    AssertNameIsAvailable name names,
    newNames ~ Insert name names
  ) =>
  SelectOptions ->
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames freePins)
    (Select name)
select opts = iliftFree $ MkSelect @name opts Select

textSensor ::
  forall name names freePins newNames boardName.
  ( KnownSymbol name,
    AssertNameIsAvailable name names,
    newNames ~ Insert name names
  ) =>
  ESPM
    (Board boardName names freePins)
    (Board boardName newNames freePins)
    (TextSensor name)
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
