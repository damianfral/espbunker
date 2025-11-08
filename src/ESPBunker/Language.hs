{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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

data Switch (name :: Symbol) (pin :: Nat) = Switch

data BinarySensor (name :: Symbol) (pin :: Nat) = BinarySensor

data Light (name :: Symbol) = Light

data Script (name :: Symbol) = Script

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

data ESPActionF f g next where
  TurnOnSwitch :: (KnownSymbol name) => Switch name pin -> next -> ESPActionF f g next
  TurnOffSwitch :: (KnownSymbol name) => Switch name pin -> next -> ESPActionF f g next
  TurnOnLight :: (KnownSymbol name) => Light name -> next -> ESPActionF f g next
  TurnOffLight :: (KnownSymbol name) => Light name -> next -> ESPActionF f g next
  RunScript :: (KnownSymbol name) => Script name -> next -> ESPActionF f g next
  LogMsg :: Text -> next -> ESPActionF f g next
  Delay :: Int -> next -> ESPActionF f g next

instance IxFunctor ESPActionF where
  imap f (TurnOnSwitch s next) = TurnOnSwitch s $ f next
  imap f (TurnOffSwitch s next) = TurnOffSwitch s $ f next
  imap f (TurnOnLight l next) = TurnOnLight l $ f next
  imap f (TurnOffLight l next) = TurnOffLight l $ f next
  imap f (RunScript sc next) = RunScript sc $ f next
  imap f (LogMsg msg next) = LogMsg msg $ f next
  imap f (Delay ms next) = Delay ms $ f next

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
    forall name names freePins newNames next.
    ( KnownSymbol name,
      AssertNameIsNotUsed name names,
      newNames ~ Insert name names
    ) =>
    next -> ESPF (Board names freePins) (Board newNames freePins) next
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

instance IxFunctor ESPF where
  imap f (MkBoard @board next) = MkBoard @board (f next)
  imap f (MkBinarySensor @name @pin options next) = MkBinarySensor @name @pin options (f next)
  imap f (MkLight @name next) = MkLight @name (f next)
  imap f (MkScript @name options next) = MkScript @name options (f next)
  imap f (MkSwitch @name @pin next) = MkSwitch @name @pin (f next)

--------------------------------------------------------------------------------

type ESPM from to a = IxFree ESPF from to a

-- esp32c3 :: forall board. ESPM board board ()
type ESP32C3 = (Board '[] '[0, 1])

esp32c3 :: ESPM ESP32C3 ESP32C3 ()
esp32c3 = iliftFree $ MkBoard @ESP32C3 ()

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
  forall name names freePins newNames.
  ( KnownSymbol name,
    AssertNameIsNotUsed name names,
    newNames ~ Insert name names
  ) =>
  ESPM (Board names freePins) (Board newNames freePins) (Light name)
light = iliftFree $ MkLight @name @names @freePins Light

--------------------------------------------------------------------------------

(>>=) :: IxFree ESPF i j a -> (a -> IxFree ESPF j k b) -> IxFree ESPF i k b
(>>=) = (>>>=) -- the indexed bind from IxFree

--------------------------------------------------------------------------------

done :: IxFree ESPF i i ()
done = ireturn ()

example' = do
  -- We are forced to use explicit bindings due to the indexed (>>>=)
  _ <- esp32c3
  r1 <- switch @"switch1" @0
  _ <-
    binarySensor @"btn1" @1
      def
        { onPress = do
            turnOn r1
            delay 1000
            turnOff r1
        }
  done
