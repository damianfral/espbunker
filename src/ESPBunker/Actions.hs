{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Actions where

import Control.Monad.Free (Free (..), foldFree, liftF)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
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

sampleSensor :: (KnownSymbol name) => Sensor name -> ESPAction ()
sampleSensor s = liftF $ SampleSensor s ()

sampleTextSensor :: (KnownSymbol name) => TextSensor name -> ESPAction ()
sampleTextSensor s = liftF $ SampleTextSensor s ()

openCover :: (KnownSymbol name) => Cover name platform -> ESPAction ()
openCover c = liftF $ OpenCover c ()

closeCover :: (KnownSymbol name) => Cover name platform -> ESPAction ()
closeCover c = liftF $ CloseCover c ()

stopCover :: (KnownSymbol name) => Cover name platform -> ESPAction ()
stopCover c = liftF $ StopCover c ()

toggleSwitch :: (KnownSymbol name) => Switch name platform pin -> ESPAction ()
toggleSwitch s = liftF $ ToggleSwitch s ()

turnOnOutput :: (KnownSymbol name) => Output name platform -> ESPAction ()
turnOnOutput o = liftF $ TurnOnOutput o ()

turnOffOutput :: (KnownSymbol name) => Output name platform -> ESPAction ()
turnOffOutput o = liftF $ TurnOffOutput o ()

setOutputValue :: (KnownSymbol name) => Output name platform -> Double -> ESPAction ()
setOutputValue o v = liftF $ SetOutputValue o v ()

setNumber :: (KnownSymbol name) => NumberComponent name -> Double -> ESPAction ()
setNumber n v = liftF $ SetNumber n v ()

incrementNumber :: (KnownSymbol name) => NumberComponent name -> Double -> ESPAction ()
incrementNumber n v = liftF $ IncrementNumber n v ()

decrementNumber :: (KnownSymbol name) => NumberComponent name -> Double -> ESPAction ()
decrementNumber n v = liftF $ DecrementNumber n v ()

--------------------------------------------------------------------------------

interpretAction :: Free ESPActionF () -> Array
interpretAction = execWriter . foldFree go
  where
    go :: ESPActionF x -> Writer Array x
    go (ToggleSwitch @name _switch next) = do
      tell [Object $ "switch.toggle" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (TurnOnSwitch @name _switch next) = do
      tell [Object $ "switch.turn_on" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (TurnOffSwitch @name _switch next) = do
      tell [Object $ "switch.turn_off" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (TurnOnLight @name _light next) = do
      tell [Object $ "light.turn_on" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (TurnOffLight @name _light next) = do
      tell [Object $ "light.turn_off" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (LogMsg msg next) = do
      tell [Object $ "logger.log" .= msg]
      pure next
    go (Delay ms next) = do
      tell [Object $ "delay" .= (show @Text ms <> "ms")]
      pure next
    go (ComponentUpdate cID next) = do
      tell [Object $ "component.update" .= cID]
      pure next
    go (ComponentSuspend cID next) = do
      tell [Object $ "component.suspend" .= cID]
      pure next
    go (IfSwitchIsOn switch thenAction elseAction next) = do
      tell $ ifAction "switch.is_on" switch thenAction elseAction
      pure next
    go (IfSwitchIsOff switch thenAction elseAction next) = do
      tell $ ifAction "switch.is_off" switch thenAction elseAction
      pure next
    go (RunScript @name _script next) = do
      tell [Object $ "script.execute" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (SetNumber @name _number val next) = do
      tell
        [ Object $ "number.set"
            .= object
              ["id" .= snakeCase (symbolVal (Proxy @name)), "value" .= val]
        ]
      pure next
    go (IncrementNumber @name _number _val next) = do
      tell [Object $ "number.increment" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (DecrementNumber @name _number _val next) = do
      tell [Object $ "number.decrement" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (SetOutputValue @name _output val next) = do
      let n = symbolVal (Proxy @name)
          setLevel = object ["id" .= snakeCase n, "level" .= val]
      tell [Object $ "output.set_level" .= setLevel]
      pure next
    go (TurnOnOutput @name _output next) = do
      tell [Object $ "output.turn_on" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (TurnOffOutput @name _output next) = do
      tell [Object $ "output.turn_off" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (OpenCover @name _cover next) = do
      tell [Object $ "cover.open" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (CloseCover @name _cover next) = do
      tell [Object $ "cover.close" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (StopCover @name _cover next) = do
      tell [Object $ "cover.stop" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (SampleSensor @name _sensor next) = do
      tell [Object $ "component.update" .= snakeCase (symbolVal (Proxy @name))]
      pure next
    go (SampleTextSensor @name _sensor next) = do
      tell [Object $ "component.update" .= snakeCase (symbolVal (Proxy @name))]
      pure next

actionBranch :: Key -> ESPAction () -> [(Key, Value)]
actionBranch key action =
  let interpreted = interpretAction action
   in [key .= interpreted | not $ null interpreted]

ifAction ::
  forall name platform pin.
  (KnownSymbol name) =>
  Key ->
  Switch name platform pin ->
  ESPAction () ->
  Maybe (ESPAction ()) ->
  Array
ifAction condName _switch thenAction elseAction =
  let n = symbolVal (Proxy @name)
      conditionPart = object [condName .= snakeCase n]
      thenPart = interpretAction thenAction
      elseMaybe = maybe [] (actionBranch "else") elseAction
   in [object ["if" .= object (["condition" .= conditionPart, "then" .= thenPart] <> elseMaybe)]]
