{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Default
import qualified Data.Text as T
import Data.Type.Bool (Not)
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import GHC.TypeError (Assert)
import GHC.TypeLits
import Relude hiding (State, natVal, return, (>>=))

--------------------------------------------------------------------------------

-- * Platform parameter

data Platform = GPIO | Out | RGB | LEDC | ESP32_PWM

type family PlatformToSymbol (platform :: Platform) :: Symbol where
  PlatformToSymbol GPIO = "gpio"
  PlatformToSymbol Out = "output"
  PlatformToSymbol RGB = "rgb"
  PlatformToSymbol LEDC = "ledc"
  PlatformToSymbol ESP32_PWM = "esp32_pwm"

--------------------------------------------------------------------------------

-- * Components

-- | Binary output https://esphome.io/components/binary_output/
data BinaryOutput (name :: Symbol) (pin :: Nat) = BinaryOutput

--------------------------------------------------------------------------------

-- | Binary sensor https://esphome.io/components/binary_sensor/
data BinarySensor (name :: Symbol) (platform :: Platform) (pin :: Nat)
  = BinarySensor

data BinarySensorOptions = BinarySensorOptions
  { onPress :: ESPAction,
    onRelease :: ESPAction,
    onClick :: ESPAction,
    onDoubleClick :: ESPAction,
    onLongPress :: ESPAction
  }
  deriving (Generic)

instance Default BinarySensorOptions where
  def = BinarySensorOptions noAction noAction noAction noAction noAction
    where
      noAction = ireturn ()

newtype BinarySensorGPIOOptions = BinarySensorGPIOOptions
  { deviceClass :: Text -- TODO: Add sum type
  }
  deriving (Generic)

instance Default BinarySensorGPIOOptions where
  def = BinarySensorGPIOOptions ""

--------------------------------------------------------------------------------

-- | Cover https://esphome.io/components/cover/
data Cover (name :: Symbol) = Cover

newtype CoverOptions = CoverOptions
  { deviceClass :: Text -- TODO: Add sum type
  }

-- | Light https://esphome.io/components/light/
data Light (name :: Symbol) = Light

data LightOptions = LightOptions
  { lightTransitionLength :: Maybe Int, -- Mapped to 'transition_length' in ms
    lightEffects :: [Text] -- Mapped to 'effects'
  }

instance Default LightOptions where
  def = LightOptions Nothing []

data LightRGBOptions where
  LightRGBOptions ::
    forall red green blue.
    (KnownSymbol red, KnownSymbol green, KnownSymbol blue) =>
    {red :: Output red, green :: Output green, blue :: Output blue} ->
    LightRGBOptions

-- | Number https://esphome.io/components/number/
data NumberComponent (name :: Symbol) = NumberComponent

data NumberOptions = NumberOptions
  { numberMin :: Maybe Double,
    numberMax :: Maybe Double,
    numberStep :: Maybe Double,
    numberUnit :: Maybe Text
  }

instance Default NumberOptions where
  def = NumberOptions Nothing Nothing Nothing Nothing

-- | Output https://esphome.io/components/output/
data Output (name :: Symbol) = Output

-- | Script https://esphome.io/components/script/
data Script (name :: Symbol) = Script

-- | Select https://esphome.io/components/select/
data Select (name :: Symbol) = Select

data SelectOptions = SelectOptions
  { selectOptions :: [Text],
    selectInitial :: Maybe Text
  }

instance Default SelectOptions where
  def = SelectOptions [] Nothing

-- | Sensor https://esphome.io/components/sensor/
data Sensor (name :: Symbol) = Sensor

data SensorOptions = SensorOptions
  { sensorUnit :: Text,
    sensorAccuracy :: Maybe Int,
    sensorIntervalMs :: Maybe Int
  }

instance Default SensorOptions where
  def = SensorOptions "" Nothing Nothing

-- | Switch https://esphome.io/components/switch/
data
  Switch -- (platform :: Platform )
    (name :: Symbol)
    (pin :: Nat)
  = Switch

--------------------------------------------------------------------------------

data ESPActionF f g next where
  CloseCover :: (KnownSymbol name) => Cover name -> next -> ESPActionF f g next
  DecrementNumber :: (KnownSymbol name) => NumberComponent name -> Double -> next -> ESPActionF f g next
  Delay :: Int -> next -> ESPActionF f g next
  IncrementNumber :: (KnownSymbol name) => NumberComponent name -> Double -> next -> ESPActionF f g next
  LogMsg :: Text -> next -> ESPActionF f g next
  OpenCover :: (KnownSymbol name) => Cover name -> next -> ESPActionF f g next
  RunScript :: (KnownSymbol name) => Script name -> next -> ESPActionF f g next
  SampleSensor :: (KnownSymbol name) => Sensor name -> next -> ESPActionF f g next
  SampleTextSensor :: (KnownSymbol name) => Sensor name -> next -> ESPActionF f g next
  SetNumber :: (KnownSymbol name) => NumberComponent name -> Double -> next -> ESPActionF f g next
  SetOutputValue :: (KnownSymbol name) => Output name -> Double -> next -> ESPActionF f g next
  StopCover :: (KnownSymbol name) => Cover name -> next -> ESPActionF f g next
  ToggleBinaryOutput :: (KnownSymbol name) => BinaryOutput name pin -> next -> ESPActionF f g next
  TurnOffLight :: (KnownSymbol name) => Light name -> next -> ESPActionF f g next
  TurnOffOutput :: (KnownSymbol name) => Output name -> next -> ESPActionF f g next
  TurnOffSwitch :: (KnownSymbol name) => Switch name pin -> next -> ESPActionF f g next
  TurnOnLight :: (KnownSymbol name) => Light name -> next -> ESPActionF f g next
  TurnOnOutput :: (KnownSymbol name) => Output name -> next -> ESPActionF f g next
  TurnOnSwitch :: (KnownSymbol name) => Switch name pin -> next -> ESPActionF f g next

instance IxFunctor ESPActionF where
  imap f (TurnOnSwitch s next) = TurnOnSwitch s $ f next
  imap f (TurnOffSwitch s next) = TurnOffSwitch s $ f next
  imap f (TurnOnLight l next) = TurnOnLight l $ f next
  imap f (TurnOffLight l next) = TurnOffLight l $ f next
  imap f (RunScript sc next) = RunScript sc $ f next
  imap f (LogMsg msg next) = LogMsg msg $ f next
  imap f (Delay ms next) = Delay ms $ f next
  imap f (SetNumber n v next) = SetNumber n v (f next)
  imap f (IncrementNumber n v next) = IncrementNumber n v (f next)
  imap f (DecrementNumber n v next) = DecrementNumber n v (f next)
  imap f (SetOutputValue n v next) = SetOutputValue n v (f next)
  imap f (TurnOnOutput n next) = TurnOnOutput n (f next)
  imap f (TurnOffOutput n next) = TurnOffOutput n (f next)
  imap f (ToggleBinaryOutput n next) = ToggleBinaryOutput n (f next)
  imap f (OpenCover n next) = OpenCover n (f next)
  imap f (CloseCover n next) = CloseCover n (f next)
  imap f (StopCover n next) = StopCover n (f next)
  imap f (SampleSensor n next) = SampleSensor n (f next)
  imap f (SampleTextSensor n next) = SampleTextSensor n (f next)

newtype IxIdentity i j a = IxIdentity a

instance IxFunctor IxIdentity where
  imap f (IxIdentity a) = IxIdentity $ f a

type ESPAction = IxFree ESPActionF () () ()

--------------------------------------------------------------------------------

turnOn :: (KnownSymbol name) => Switch name pin -> ESPAction
turnOn s = iliftFree $ TurnOnSwitch s ()

turnOff :: (KnownSymbol name) => Switch name pin -> ESPAction
turnOff s = iliftFree $ TurnOffSwitch s ()

turnOnL :: (KnownSymbol name) => Light name -> ESPAction
turnOnL l = iliftFree $ TurnOnLight l ()

turnOffL :: (KnownSymbol name) => Light name -> ESPAction
turnOffL l = iliftFree $ TurnOffLight l ()

runScript :: (KnownSymbol name) => Script name -> ESPAction
runScript s = iliftFree $ RunScript s ()

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
  PlatformToOptions BinarySensor GPIO = BinarySensorGPIOOptions
  PlatformToOptions Switch GPIO = BinarySensorGPIOOptions
  PlatformToOptions Light RGB = LightRGBOptions

--------------------------------------------------------------------------------

-- The combined type-level state that the IxFree will index
-- Define Board as a kind with two type-level lists
data Board (names :: [Symbol]) (pins :: [Nat]) = Board

--------------------------------------------------------------------------------

-- * DSL Functor

data ESPF :: Type -> Type -> Type -> Type where
  MkBoard :: forall board next. next -> ESPF board board next
  MkBinarySensor ::
    forall name platform pin names freePins newNames newFreePins options platformSymbol next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertNameIsNotUsed name names,
      AssertPinIsAvailable pin freePins,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins,
      options ~ PlatformToOptions BinarySensor platform,
      KeyMapOptions options,
      platformSymbol ~ PlatformToSymbol platform,
      KnownSymbol platformSymbol
    ) =>
    BinarySensorOptions ->
    options ->
    next ->
    ESPF (Board names freePins) (Board newNames newFreePins) next
  MkLight ::
    forall name platform names freePins newNames options platformSymbol next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      platformSymbol ~ PlatformToSymbol platform,
      KnownSymbol platformSymbol,
      KeyMapOptions options,
      options ~ PlatformToOptions Light platform,
      KeyMapOptions options
    ) =>
    LightOptions ->
    options ->
    next ->
    ESPF (Board names freePins) (Board newNames freePins) next
  MkScript ::
    forall name names freePins newNames next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    ESPAction ->
    next ->
    ESPF (Board names freePins) (Board newNames freePins) next
  MkSwitch ::
    forall name pin names freePins newNames newFreePins next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins
    ) =>
    next -> ESPF (Board names freePins) (Board newNames newFreePins) next
  MkSensor ::
    forall name names freePins newNames next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    SensorOptions ->
    next ->
    ESPF (Board names freePins) (Board newNames freePins) next
  MkTextSensor ::
    forall name names freePins newNames next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    next ->
    ESPF (Board names freePins) (Board newNames freePins) next
  MkNumber ::
    forall name names freePins newNames next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    NumberOptions ->
    next ->
    ESPF (Board names freePins) (Board newNames freePins) next
  MkOutput ::
    forall name pin names freePins newNames newFreePins next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins
    ) =>
    next ->
    ESPF (Board names freePins) (Board newNames newFreePins) next
  MkBinaryOutput ::
    forall name pin names freePins newNames newFreePins next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins
    ) =>
    next ->
    ESPF (Board names freePins) (Board newNames newFreePins) next
  MkCover ::
    forall name platform names freePins newNames newFreePins next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      KnownSymbol (PlatformToSymbol platform)
    ) =>
    next ->
    ESPF (Board names freePins) (Board newNames newFreePins) next
  MkButton ::
    forall name pin names freePins newNames newFreePins next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertPinIsAvailable pin freePins,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins
    ) =>
    next ->
    ESPF (Board names freePins) (Board newNames newFreePins) next

instance IxFunctor ESPF where
  imap f (MkBinaryOutput @name @pin next) = MkBinaryOutput @name @pin (f next)
  imap f (MkBinarySensor @name @platform @pin options platformOptions next) =
    MkBinarySensor @name @platform @pin options platformOptions (f next)
  imap f (MkBoard @board next) = MkBoard @board (f next)
  imap f (MkButton @name @pin next) = MkButton @name @pin (f next)
  imap f (MkCover @name @platform next) = MkCover @name @platform (f next)
  imap f (MkLight @name @platform options platformOptions next) =
    MkLight @name @platform options platformOptions (f next)
  imap f (MkNumber @name options next) = MkNumber @name options (f next)
  imap f (MkOutput @name @pin next) = MkOutput @name @pin (f next)
  imap f (MkScript @name options next) = MkScript @name options (f next)
  imap f (MkSensor @name options next) = MkSensor @name options (f next)
  imap f (MkSwitch @name @pin next) = MkSwitch @name @pin (f next)
  imap f (MkTextSensor @name next) = MkTextSensor @name (f next)

--------------------------------------------------------------------------------

type ESPM from to a = IxFree ESPF from to a

-- esp32c3 :: forall board. ESPM board board ()
type ESP32C3 = (Board '[] '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

board :: forall board names pins. ESPM board board (Board names pins)
board = iliftFree $ MkBoard @board Board

switch ::
  forall name pin names freePins newNames newFreePins.
  ( KnownSymbol name,
    KnownNat pin,
    AssertNameIsNotUsed name names,
    AssertPinIsAvailable pin freePins,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins
  ) =>
  ESPM (Board names freePins) (Board newNames newFreePins) (Switch name pin)
switch =
  iliftFree
    $ MkSwitch @name @pin @names @freePins @newNames @newFreePins Switch

binarySensor ::
  forall name platform pin names freePins newNames newFreePins options.
  ( KnownSymbol name,
    KnownNat pin,
    AssertNameIsNotUsed name names,
    AssertPinIsAvailable pin freePins,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins,
    options ~ PlatformToOptions BinarySensor platform,
    KnownSymbol (PlatformToSymbol platform),
    KeyMapOptions options
  ) =>
  BinarySensorOptions ->
  options ->
  ESPM
    (Board names freePins)
    (Board newNames newFreePins)
    (BinarySensor name platform pin)
binarySensor options platformOptions =
  iliftFree
    $ MkBinarySensor
      @name
      @platform
      @pin
      @names
      @freePins
      @newNames
      @newFreePins
      options
      platformOptions
      BinarySensor

script ::
  forall name names freePins newNames.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  ESPAction ->
  ESPM (Board names freePins) (Board newNames freePins) (Script name)
script actions = iliftFree $ MkScript @name @names @freePins actions Script

light ::
  forall name platform names freePins newNames options platformSymbol.
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
  ESPM (Board names freePins) (Board newNames freePins) (Light name)
light options platformOptions =
  iliftFree
    $ MkLight
      @name
      @platform
      @names
      @freePins
      @newNames
      options
      platformOptions
      Light

sensor ::
  forall name names freePins newNames.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  SensorOptions ->
  IxFree ESPF (Board names freePins) (Board newNames freePins) (Sensor name)
sensor opts = iliftFree (MkSensor @name opts Sensor)

output ::
  forall name pin names freePins newNames newFreePins.
  ( KnownSymbol name,
    KnownNat pin,
    AssertPinIsAvailable pin freePins,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins
  ) =>
  IxFree ESPF (Board names freePins) (Board newNames newFreePins) (Output name)
output = iliftFree (MkOutput @name @pin Output)

cover ::
  forall name platform names freePins newNames newFreePins.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names,
    KnownSymbol (PlatformToSymbol platform)
  ) =>
  IxFree ESPF (Board names freePins) (Board newNames newFreePins) (Cover name)
cover = iliftFree (MkCover @name @platform Cover)

--------------------------------------------------------------------------------

(>>=) :: IxFree ESPF i j a -> (a -> IxFree ESPF j k b) -> IxFree ESPF i k b
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
  TurnOnSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "switch.turn_on" $ String (T.pack n)
     in [option] <> interpretAction next
  TurnOffSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "switch.turn_off" $ String (T.pack n)
     in [option] <> interpretAction next
  TurnOnLight @name _light next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "light.turn_on" $ String (T.pack n)
     in [option] <> interpretAction next
  TurnOffLight @name _light next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "light.turn_off" $ String (T.pack n)
     in [option] <> interpretAction next
  LogMsg msg next ->
    let option = Object $ KM.singleton "logger.log" $ String msg
     in [option] <> interpretAction next
  Delay ms next ->
    let option = Object $ KM.singleton "delay" $ String (show ms <> "ms")
     in [option] <> interpretAction next
  RunScript @name _script next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "script.execute" $ String (T.pack n)
     in [option] <> interpretAction next
  SetNumber @name _number val next ->
    let n = symbolVal (Proxy @name)
        yamlNode =
          Object
            $ KM.singleton "number.set"
            $ object
              [ ("id", String (T.pack n)),
                ("value", String (show val))
              ]
     in [yamlNode] <> interpretAction next
  IncrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "number.increment" $ String (T.pack n)
     in [option] <> interpretAction next
  DecrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "number.decrement" $ String (T.pack n)
     in [option] <> interpretAction next
  SetOutputValue @name _output val next ->
    let n = symbolVal (Proxy @name)
        yamlNode =
          Object
            $ KM.singleton "output.set_level"
            $ object
              [ ("id", String (T.pack n)),
                ("level", String (T.pack (show val)))
              ]
     in [yamlNode] <> interpretAction next
  TurnOnOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "output.turn_on" $ String (T.pack n)
     in [option] <> interpretAction next
  TurnOffOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "output.turn_off" $ String (T.pack n)
     in [option] <> interpretAction next
  ToggleBinaryOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "switch.toggle" $ String (T.pack n)
     in [option] <> interpretAction next
  OpenCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "cover.open" $ String (T.pack n)
     in [option] <> interpretAction next
  CloseCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "cover.close" $ String (T.pack n)
     in [option] <> interpretAction next
  StopCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "cover.stop" $ String (T.pack n)
     in [option] <> interpretAction next
  SampleSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "component.update" $ String (T.pack n)
     in [option] <> interpretAction next
  SampleTextSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        option = Object $ KM.singleton "component.update" $ String (T.pack n)
     in [option] <> interpretAction next

--------------------------------------------------------------------------------

-- * Main interpreter

interpretESP :: IxFree ESPF i j a -> [Value]
interpretESP (Pure _) = []
interpretESP (Free espf) =
  case espf of
    MkBoard next -> interpretESP next
    MkSwitch @name @pin next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "switch",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", toJSON $ pinToText @pin),
                      ("name", toJSON n)
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkCover @name next ->
      let n = symbolVal (Proxy @name)
          coverNode =
            object
              [ ( "cover",
                  object
                    [ ("platform", String "template"),
                      ("name", String (T.pack n))
                    ]
                )
              ]
       in coverNode : interpretESP next
    MkButton @name @pin next ->
      let n = symbolVal (Proxy @name)
          buttonNode =
            object
              [ ( "button",
                  object
                    [ ("platform", String "template"),
                      ("name", toJSON n)
                    ]
                )
              ]
          binarySensorNode =
            object
              [ ( "binary_sensor",
                  object
                    [ ("platform", String "gpio"),
                      ("pin", String $ pinToText @pin),
                      ("name", toJSON n),
                      ( "on_press",
                        Array $ V.fromList [object [("button.press", toJSON n)]]
                      )
                    ]
                )
              ]
       in buttonNode : binarySensorNode : interpretESP next
    MkOutput @name @pin next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "output",
                  object
                    [ ("platform", String "gpio"),
                      ("name", toJSON n),
                      ("pin", toJSON $ pinToText @pin)
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkBinaryOutput @name @pin next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "switch",
                  object
                    [ ("platform", String "gpio"),
                      ("name", String (T.pack n)),
                      ("pin", toJSON $ pinToText @pin)
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkNumber @name options next ->
      let n = symbolVal (Proxy @name)
          opts =
            [ ("platform", String "template"),
              ("name", String (T.pack n))
            ]
              <> catMaybes
                [ ("min_value",) . String . T.pack . show <$> numberMin options,
                  ("max_value",) . String . T.pack . show <$> numberMax options,
                  ("step",) . String . T.pack . show <$> numberStep options,
                  ("unit_of_measurement",) . String <$> numberUnit options
                ]
          yamlNode = object [("number", object opts)]
       in yamlNode : interpretESP next
    MkSensor @name options next ->
      let n = symbolVal (Proxy @name)
          opts =
            [ ("platform", String "template"),
              ("name", String (T.pack n)),
              ("unit_of_measurement", String (sensorUnit options))
            ]
              <> catMaybes
                [ do
                    accuracy <- sensorAccuracy options
                    Just ("accuracy_decimals", String $ show accuracy),
                  do
                    interval <- sensorIntervalMs options
                    Just ("update_interval", String $ show interval <> "ms")
                ]
          yamlNode = object [("sensor", object opts)]
       in yamlNode : interpretESP next
    MkTextSensor @name next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "text_sensor",
                  object
                    [ ("platform", String "template"),
                      ("name", String (T.pack n))
                    ]
                )
              ]
       in yamlNode : interpretESP next
    MkBinarySensor @name @platform @pin options platformOptions next ->
      let n = symbolVal (Proxy @name)
          platform = symbolVal (Proxy @(PlatformToSymbol platform))
          yamlPlatformNode = toKeyMap platformOptions
          yamlNode =
            Object
              $ KM.singleton "binary_sensor"
              $ Object
              $ KM.fromList
                [ ("platform", String $ toText platform),
                  ("pin", toJSON $ pinToText @pin),
                  ("name", String (T.pack n)),
                  ("on_press", Array $ interpretAction $ onPress options)
                ]
              <> yamlPlatformNode
       in [yamlNode] <> interpretESP next
    MkLight @name @platform _options platformOptions next ->
      let n = symbolVal $ Proxy @name
          platform = symbolVal $ Proxy @(PlatformToSymbol platform)
          yamlPlatformNode = toKeyMap platformOptions
          yamlNode =
            object
              [ ( "light",
                  Object
                    $ KM.fromList
                      [ ("platform", String $ toText platform),
                        ("name", String (T.pack n))
                      ]
                    <> yamlPlatformNode
                )
              ]
       in [yamlNode] <> interpretESP next
    MkScript @name action next ->
      let n = symbolVal (Proxy @name)
          yamlNode =
            object
              [ ( "script",
                  object
                    [ ("name", String (T.pack n)),
                      ("then", Array $ interpretAction action)
                    ]
                )
              ]
       in yamlNode : interpretESP next

--------------------------------------------------------------------------------

generateYAML :: IxFree ESPF i j a -> ByteString
generateYAML prog =
  let nodes = interpretESP prog in YAML.encode $ Array $ V.fromList nodes

class KeyMapOptions a where toKeyMap :: a -> KeyMap Value

instance KeyMapOptions BinarySensorGPIOOptions where
  toKeyMap (BinarySensorGPIOOptions deviceClass) =
    KM.singleton "device_class" $ String deviceClass

instance KeyMapOptions BinarySensorOptions where
  toKeyMap BinarySensorOptions {..} =
    KM.fromList
      [ mapAction "on_press" onPress,
        mapAction "on_release" onRelease,
        mapAction "on_click" onClick,
        mapAction "on_double_click" onDoubleClick,
        mapAction "on_multi_click" onLongPress
      ]
    where
      mapAction :: Text -> ESPAction -> (Key, Value)
      mapAction key action =
        (fromString $ toString key, Array $ interpretAction action)

instance KeyMapOptions LightRGBOptions where
  toKeyMap (LightRGBOptions @red @green @blue _red _green _blue) =
    KM.fromList
      [ ("r", toJSON $ symbolVal $ Proxy @red),
        ("g", toJSON $ symbolVal $ Proxy @green),
        ("b", toJSON $ symbolVal $ Proxy @blue)
      ]

instance KeyMapOptions LightOptions where
  toKeyMap LightOptions {..} =
    KM.fromList
      $ catMaybes
        [ mapIntOption "transition_length" lightTransitionLength,
          mapEffectsOption "effects" lightEffects
        ]
    where
      -- Helper to include an optional Int field as a String value (e.g., "100ms")
      mapIntOption :: Text -> Maybe Int -> Maybe (Key, Value)
      mapIntOption key val = do
        transitionL <- val
        Just (fromString $ toString key, String $ show transitionL <> "ms")

      -- Helper to include a list of effects only if the list is not empty
      mapEffectsOption :: Text -> [Text] -> Maybe (Key, Value)
      mapEffectsOption key effects = do
        guard $ not $ null effects
        let k' = fromString $ toString key
        Just (k', Array $ V.fromList $ String <$> effects)

pinToText :: forall pin. (KnownNat pin) => Text
pinToText =
  let p = natVal (Proxy @pin)
   in "GPIO" <> show p
