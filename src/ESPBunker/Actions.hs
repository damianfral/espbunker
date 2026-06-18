{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Actions where

import Control.Monad.Free (Free (..), liftF)
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Proxy
import ESPBunker.Components
import GHC.TypeLits
import Relude hiding (State, natVal, return)

--------------------------------------------------------------------------------

data ESPActionF next where
  CloseCover ::
    (KnownSymbol name) =>
    Cover name platform -> next -> ESPActionF next
  DecrementNumber ::
    (KnownSymbol name) =>
    NumberComponent name -> Double -> next -> ESPActionF next
  Delay :: Int -> next -> ESPActionF next
  ComponentUpdate ::
    Text -> next -> ESPActionF next
  ComponentSuspend ::
    Text -> next -> ESPActionF next
  IfSwitchIsOn ::
    (KnownSymbol name) =>
    Switch name platform pin ->
    ESPAction () ->
    Maybe (ESPAction ()) ->
    next ->
    ESPActionF next
  IfSwitchIsOff ::
    (KnownSymbol name) =>
    Switch name platform pin ->
    ESPAction () ->
    Maybe (ESPAction ()) ->
    next ->
    ESPActionF next
  IncrementNumber ::
    (KnownSymbol name) =>
    NumberComponent name -> Double -> next -> ESPActionF next
  LogMsg :: Text -> next -> ESPActionF next
  OpenCover ::
    (KnownSymbol name) =>
    Cover name platform -> next -> ESPActionF next
  RunScript ::
    (KnownSymbol name) =>
    Script name -> next -> ESPActionF next
  SampleSensor ::
    (KnownSymbol name) =>
    Sensor name -> next -> ESPActionF next
  SampleTextSensor ::
    (KnownSymbol name) =>
    TextSensor name -> next -> ESPActionF next
  SetNumber ::
    (KnownSymbol name) =>
    NumberComponent name -> Double -> next -> ESPActionF next
  SetOutputValue ::
    (KnownSymbol name) =>
    Output name platform -> Double -> next -> ESPActionF next
  StopCover ::
    (KnownSymbol name) =>
    Cover name platform -> next -> ESPActionF next
  ToggleSwitch ::
    (KnownSymbol name) =>
    Switch name platform pin -> next -> ESPActionF next
  TurnOffLight ::
    (KnownSymbol name) =>
    Light name platform -> next -> ESPActionF next
  TurnOffOutput ::
    (KnownSymbol name) =>
    Output name platform -> next -> ESPActionF next
  TurnOffSwitch ::
    (KnownSymbol name) =>
    Switch name platform pin -> next -> ESPActionF next
  TurnOnLight ::
    (KnownSymbol name) =>
    Light name platform -> next -> ESPActionF next
  TurnOnOutput ::
    (KnownSymbol name) =>
    Output name platform -> next -> ESPActionF next
  TurnOnSwitch ::
    (KnownSymbol name) =>
    Switch name platform pin -> next -> ESPActionF next

instance Functor ESPActionF where
  fmap f (CloseCover n next) = CloseCover n $ f next
  fmap f (DecrementNumber n v next) = DecrementNumber n v $ f next
  fmap f (Delay ms next) = Delay ms $ f next
  fmap f (ComponentUpdate cID next) = ComponentUpdate cID $ f next
  fmap f (ComponentSuspend cID next) = ComponentSuspend cID $ f next
  fmap f (IfSwitchIsOn sw thenAct elseAct next) =
    IfSwitchIsOn sw thenAct elseAct $ f next
  fmap f (IfSwitchIsOff sw thenAct elseAct next) =
    IfSwitchIsOff sw thenAct elseAct $ f next
  fmap f (IncrementNumber n v next) = IncrementNumber n v $ f next
  fmap f (LogMsg msg next) = LogMsg msg $ f next
  fmap f (OpenCover n next) = OpenCover n $ f next
  fmap f (RunScript sc next) = RunScript sc $ f next
  fmap f (SampleSensor n next) = SampleSensor n $ f next
  fmap f (SampleTextSensor n next) = SampleTextSensor n $ f next
  fmap f (SetNumber n v next) = SetNumber n v $ f next
  fmap f (SetOutputValue n v next) = SetOutputValue n v $ f next
  fmap f (StopCover n next) = StopCover n $ f next
  fmap f (ToggleSwitch n next) = ToggleSwitch n $ f next
  fmap f (TurnOffLight l next) = TurnOffLight l $ f next
  fmap f (TurnOffOutput n next) = TurnOffOutput n $ f next
  fmap f (TurnOffSwitch s next) = TurnOffSwitch s $ f next
  fmap f (TurnOnLight l next) = TurnOnLight l $ f next
  fmap f (TurnOnOutput n next) = TurnOnOutput n $ f next
  fmap f (TurnOnSwitch s next) = TurnOnSwitch s $ f next

type ESPAction = Free ESPActionF

noAction :: ESPAction ()
noAction = pure ()

--------------------------------------------------------------------------------

turnOn :: (KnownSymbol name) => Switch name platform pin -> ESPAction ()
turnOn s = liftF $ TurnOnSwitch s ()

turnOff :: (KnownSymbol name) => Switch name platform pin -> ESPAction ()
turnOff s = liftF $ TurnOffSwitch s ()

turnOnL :: (KnownSymbol name) => Light name platform -> ESPAction ()
turnOnL l = liftF $ TurnOnLight l ()

turnOffL :: (KnownSymbol name) => Light name platform -> ESPAction ()
turnOffL l = liftF $ TurnOffLight l ()

runScript :: (KnownSymbol name) => Script name -> ESPAction ()
runScript s = liftF $ RunScript s ()

componentUpdate :: Text -> ESPAction ()
componentUpdate cID = liftF $ ComponentUpdate cID ()

componentSuspend :: Text -> ESPAction ()
componentSuspend cID = liftF $ ComponentSuspend cID ()

ifSwitchIsOn ::
  (KnownSymbol name) =>
  Switch name platform pin -> ESPAction () -> Maybe (ESPAction ()) -> ESPAction ()
ifSwitchIsOn sw thenAction elseAction = liftF $ IfSwitchIsOn sw thenAction elseAction ()

ifSwitchIsOff ::
  (KnownSymbol name) =>
  Switch name platform pin -> ESPAction () -> Maybe (ESPAction ()) -> ESPAction ()
ifSwitchIsOff sw thenAction elseAction = liftF $ IfSwitchIsOff sw thenAction elseAction ()

log :: Text -> ESPAction ()
log t = liftF $ LogMsg t ()

delay :: Int -> ESPAction ()
delay ms = liftF $ Delay ms ()

--------------------------------------------------------------------------------

interpretAction :: Free ESPActionF () -> Array
interpretAction (Pure _) = empty
interpretAction (Free espf) = case espf of
  ToggleSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "switch.toggle" .= snakeCase n
     in [option] <> interpretAction next
  TurnOnSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "switch.turn_on" .= snakeCase n
     in [option] <> interpretAction next
  TurnOffSwitch @name _switch next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "switch.turn_off" .= snakeCase n
     in [option] <> interpretAction next
  TurnOnLight @name _light next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "light.turn_on" .= snakeCase n
     in [option] <> interpretAction next
  TurnOffLight @name _light next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "light.turn_off" .= snakeCase n
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
        conditionPart = object ["switch.is_on" .= snakeCase n]
        thenPart = interpretAction thenAction
        elseMaybe = maybe [] (actionBranch "else") elseAction
        ifObject =
          object
            $ ["condition" .= conditionPart, "then" .= thenPart]
            <> elseMaybe
        ifAction = object ["if" .= ifObject]
     in [ifAction] <> interpretAction next
  IfSwitchIsOff @name _switch thenAction elseAction next ->
    let n = symbolVal (Proxy @name)
        conditionPart = object ["switch.is_off" .= snakeCase n]
        thenPart = interpretAction thenAction
        elseMaybe = maybe [] (actionBranch "else") elseAction
        ifObject =
          object
            $ ["condition" .= conditionPart, "then" .= thenPart]
            <> elseMaybe
        ifAction = object ["if" .= ifObject]
     in [ifAction] <> interpretAction next
  RunScript @name _script next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "script.execute" .= snakeCase n
     in [option] <> interpretAction next
  SetNumber @name _number val next ->
    let n = symbolVal (Proxy @name)
        yamlNode =
          Object $ "number.set" .= object ["id" .= snakeCase n, "value" .= val]
     in [yamlNode] <> interpretAction next
  IncrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "number.increment" .= snakeCase n
     in [option] <> interpretAction next
  DecrementNumber @name _number _val next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "number.decrement" .= snakeCase n
     in [option] <> interpretAction next
  SetOutputValue @name _output val next ->
    let n = symbolVal (Proxy @name)
        setLevel = object ["id" .= snakeCase n, "level" .= val]
        yamlNode = Object $ "output.set_level" .= setLevel
     in [yamlNode] <> interpretAction next
  TurnOnOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "output.turn_on" .= snakeCase n
     in [option] <> interpretAction next
  TurnOffOutput @name _output next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "output.turn_off" .= snakeCase n
     in [option] <> interpretAction next
  OpenCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "cover.open" .= snakeCase n
     in [option] <> interpretAction next
  CloseCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "cover.close" .= snakeCase n
     in [option] <> interpretAction next
  StopCover @name _cover next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "cover.stop" .= snakeCase n
     in [option] <> interpretAction next
  SampleSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "component.update" .= snakeCase n
     in [option] <> interpretAction next
  SampleTextSensor @name _sensor next ->
    let n = symbolVal (Proxy @name)
        option = Object $ "component.update" .= snakeCase n
     in [option] <> interpretAction next

actionBranch :: Key -> ESPAction () -> [(Key, Value)]
actionBranch key action =
  let interpreted = interpretAction action
   in [key .= interpreted | not $ null interpreted]
