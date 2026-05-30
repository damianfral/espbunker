{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Actions where

import Control.Monad.Indexed (IxFunctor (imap), ireturn)
import Control.Monad.Indexed.Free (IxFree (..), iliftFree)
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Proxy
import ESPBunker.Components
import GHC.TypeLits
import Relude hiding (State, natVal, return)

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
    Text -> next -> ESPActionF f g next
  ComponentSuspend ::
    Text -> next -> ESPActionF f g next
  IfSwitchIsOn ::
    (KnownSymbol name) =>
    Switch name platform pin ->
    ESPAction ->
    Maybe ESPAction ->
    next ->
    ESPActionF f g next
  IfSwitchIsOff ::
    (KnownSymbol name) =>
    Switch name platform pin ->
    ESPAction ->
    Maybe ESPAction ->
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
    TextSensor name -> next -> ESPActionF f g next
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
  imap f (IfSwitchIsOn sw thenAct elseAct next) =
    IfSwitchIsOn sw (imap id thenAct) (fmap (imap id) elseAct) $ f next
  imap f (IfSwitchIsOff sw thenAct elseAct next) =
    IfSwitchIsOff sw (imap id thenAct) (fmap (imap id) elseAct) $ f next
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

noAction :: ESPAction
noAction = ireturn ()

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

ifSwitchIsOn ::
  (KnownSymbol name) =>
  Switch name platform pin -> ESPAction -> Maybe ESPAction -> ESPAction
ifSwitchIsOn sw thenAction elseAction = iliftFree $ IfSwitchIsOn sw thenAction elseAction ()

ifSwitchIsOff ::
  (KnownSymbol name) =>
  Switch name platform pin -> ESPAction -> Maybe ESPAction -> ESPAction
ifSwitchIsOff sw thenAction elseAction = iliftFree $ IfSwitchIsOff sw thenAction elseAction ()

log :: Text -> ESPAction
log t = iliftFree $ LogMsg t ()

delay :: Int -> ESPAction
delay ms = iliftFree $ Delay ms ()

--------------------------------------------------------------------------------

interpretAction :: IxFree ESPActionF i j () -> Array
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

actionBranch :: Key -> ESPAction -> [(Key, Value)]
actionBranch key action =
  let interpreted = interpretAction action
   in [key .= interpreted | not $ null interpreted]
