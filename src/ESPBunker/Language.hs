{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Language where

import Control.Monad.Indexed (IxFunctor (imap), ireturn, (>>>=))
import Control.Monad.Indexed.Free (IxFree, iliftFree)
import Data.Default
import Data.Type.Bool (Not)
import GHC.TypeError (Assert)
import GHC.TypeLits
import Relude hiding (State, return, (>>=))

--------------------------------------------------------------------------------

-- * Components

-- | Binary output https://esphome.io/components/binary_output/
data BinaryOutput (name :: Symbol) (pin :: Nat) = BinaryOutput

-- | Binary sensor https://esphome.io/components/binary_sensor/
data BinarySensor (name :: Symbol) (pin :: Nat) = BinarySensor

data BinarySensorOptions = BinarySensorOptions
  { onPress :: ESPAction (),
    onRelease :: ESPAction (),
    onClick :: ESPAction (),
    onDoubleClick :: ESPAction (),
    onLongPress :: ESPAction ()
  }

instance Default BinarySensorOptions where
  def = BinarySensorOptions noAction noAction noAction noAction noAction
    where
      noAction = ireturn ()

-- | Cover https://esphome.io/components/cover/
data Cover (name :: Symbol) = Cover

-- | Light https://esphome.io/components/light/
data Light (name :: Symbol) = Light

-- | Number https://esphome.io/components/number/
data Number (name :: Symbol) = Number

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

-- data Platform = GPIO | Template | Output

--------------------------------------------------------------------------------

data ESPActionF f g next where
  CloseCover :: (KnownSymbol name) => Cover name -> next -> ESPActionF f g next
  DecrementNumber :: (KnownSymbol name) => Number name -> Double -> next -> ESPActionF f g next
  Delay :: Int -> next -> ESPActionF f g next
  IncrementNumber :: (KnownSymbol name) => Number name -> Double -> next -> ESPActionF f g next
  LogMsg :: Text -> next -> ESPActionF f g next
  OpenCover :: (KnownSymbol name) => Cover name -> next -> ESPActionF f g next
  RunScript :: (KnownSymbol name) => Script name -> next -> ESPActionF f g next
  SampleSensor :: (KnownSymbol name) => Sensor name -> next -> ESPActionF f g next
  SampleTextSensor :: (KnownSymbol name) => Sensor name -> next -> ESPActionF f g next
  SetNumber :: (KnownSymbol name) => Number name -> Double -> next -> ESPActionF f g next
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

type ESPAction = IxFree ESPActionF () ()

--------------------------------------------------------------------------------

turnOn :: (KnownSymbol name) => Switch name pin -> ESPAction ()
turnOn s = iliftFree $ TurnOnSwitch s ()

turnOff :: (KnownSymbol name) => Switch name pin -> ESPAction ()
turnOff s = iliftFree $ TurnOffSwitch s ()

turnOnL :: (KnownSymbol name) => Light name -> ESPAction ()
turnOnL l = iliftFree $ TurnOnLight l ()

turnOffL :: (KnownSymbol name) => Light name -> ESPAction ()
turnOffL l = iliftFree $ TurnOffLight l ()

runScript :: (KnownSymbol name) => Script name -> ESPAction ()
runScript s = iliftFree $ RunScript s ()

log :: Text -> ESPAction ()
log t = iliftFree $ LogMsg t ()

delay :: Int -> ESPAction ()
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

-- The combined type-level state that the IxFree will index
-- Define Board as a kind with two type-level lists
data Board (names :: [Symbol]) (pins :: [Nat]) = Board

--------------------------------------------------------------------------------

-- * DSL Functor

data ESPF :: Type -> Type -> Type -> Type where
  MkBoard :: forall board next. next -> ESPF board board next
  MkBinarySensor ::
    forall name pin names freePins newNames newFreePins next.
    ( KnownSymbol name,
      KnownNat pin,
      AssertNameIsNotUsed name names,
      AssertPinIsAvailable pin freePins,
      newNames ~ Insert name names,
      newFreePins ~ Remove pin freePins
    ) =>
    BinarySensorOptions ->
    next ->
    ESPF (Board names freePins) (Board newNames newFreePins) next
  MkLight ::
    forall name outputName names freePins newNames next.
    ( KnownSymbol name,
      KnownSymbol outputName,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    (Output outputName) ->
    next ->
    ESPF (Board names freePins) (Board newNames freePins) next
  MkScript ::
    forall name names freePins newNames next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    ESPAction () ->
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
    forall name open close names freePins newNames newFreePins next.
    ( KnownSymbol name,
      KnownNat open,
      KnownNat close,
      AssertPinIsAvailable open freePins,
      AssertPinIsAvailable close freePins,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names,
      newFreePins ~ Remove close (Remove open freePins)
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
  imap f (MkBinarySensor @name @pin options next) =
    MkBinarySensor @name @pin options (f next)
  imap f (MkBoard @board next) = MkBoard @board (f next)
  imap f (MkButton @name @pin next) = MkButton @name @pin (f next)
  imap f (MkCover @name @open @close next) = MkCover @name @open @close (f next)
  imap f (MkLight @name outp next) = MkLight @name outp (f next)
  imap f (MkNumber @name options next) = MkNumber @name options (f next)
  imap f (MkOutput @name @pin next) = MkOutput @name @pin (f next)
  imap f (MkScript @name options next) = MkScript @name options (f next)
  imap f (MkSensor @name options next) = MkSensor @name options (f next)
  imap f (MkSwitch @name @pin next) = MkSwitch @name @pin (f next)
  imap f (MkTextSensor @name next) = MkTextSensor @name (f next)

--------------------------------------------------------------------------------

type ESPM from to a = IxFree ESPF from to a

-- esp32c3 :: forall board. ESPM board board ()
type ESP32C3 = (Board '[] '[0, 1])

board :: forall board. ESPM board board ()
board = iliftFree $ MkBoard @board ()

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
  forall name pin names freePins newNames newFreePins.
  ( KnownSymbol name,
    KnownNat pin,
    AssertNameIsNotUsed name names,
    AssertPinIsAvailable pin freePins,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins
  ) =>
  BinarySensorOptions ->
  ESPM
    (Board names freePins)
    (Board newNames newFreePins)
    (BinarySensor name pin)
binarySensor options =
  iliftFree
    $ MkBinarySensor
      @name
      @pin
      @names
      @freePins
      @newNames
      @newFreePins
      options
      BinarySensor

script ::
  forall name names freePins newNames.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  ESPAction () ->
  ESPM (Board names freePins) (Board newNames freePins) (Script name)
script actions = iliftFree $ MkScript @name @names @freePins actions Script

light ::
  forall name names freePins newNames outputName.
  ( KnownSymbol name,
    KnownSymbol outputName,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  Output outputName ->
  ESPM (Board names freePins) (Board newNames freePins) (Light name)
light outp =
  iliftFree $ MkLight @name @outputName @names @freePins @newNames outp Light

sensor ::
  forall name names freePins newNames.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  SensorOptions ->
  IxFree ESPF (Board names freePins) (Board newNames freePins) ()
sensor opts = iliftFree (MkSensor @name opts ())

output ::
  forall name pin names freePins newNames newFreePins.
  ( KnownSymbol name,
    KnownNat pin,
    AssertPinIsAvailable pin freePins,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names,
    newFreePins ~ Remove pin freePins
  ) =>
  IxFree ESPF (Board names freePins) (Board newNames newFreePins) ()
output = iliftFree (MkOutput @name @pin ())

cover ::
  forall name open close names freePins newNames newFreePins.
  ( KnownSymbol name,
    KnownNat open,
    KnownNat close,
    AssertPinIsAvailable open freePins,
    AssertPinIsAvailable close freePins,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names,
    newFreePins ~ Remove close (Remove open freePins)
  ) =>
  IxFree ESPF (Board names freePins) (Board newNames newFreePins) ()
cover = iliftFree (MkCover @name @open @close ())

--------------------------------------------------------------------------------

(>>=) :: IxFree ESPF i j a -> (a -> IxFree ESPF j k b) -> IxFree ESPF i k b
(>>=) = (>>>=) -- the indexed bind from IxFree

--------------------------------------------------------------------------------

done :: ESPM i i ()
done = ireturn ()
