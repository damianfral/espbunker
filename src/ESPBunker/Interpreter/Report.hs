{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Interpreter.Report (generateReport) where

import Control.Monad.Free (Free (..))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Aeson (Value (String))
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy
import Data.Text qualified as T
import ESPBunker.Actions (ESPAction, ESPActionF (..))
import ESPBunker.Components (PlatformToSymbol)
import ESPBunker.DSL
import ESPBunker.Interpreter.Fold (ifoldFree)
import ESPBunker.KeyMapOptions (toKeyMap)
import ESPBunker.Options
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Relude

--------------------------------------------------------------------------------

data ComponentReport = ComponentReport
  { componentName :: Text,
    componentKind :: Text,
    componentPlatform :: Maybe Text
  }

data PinReport = PinReport
  { pinType :: Text,
    pinNumber :: Int,
    pinName :: Text
  }

data ScriptReport = ScriptReport
  { scriptName :: Text,
    scriptAction :: ESPAction ()
  }

data AutomationReport = AutomationReport
  { automationSource :: Text,
    automationEvent :: Text,
    automationAction :: ESPAction ()
  }

data Report = Report
  { reportComponents :: [ComponentReport],
    reportPins :: [PinReport],
    reportScripts :: [ScriptReport],
    reportAutomations :: [AutomationReport],
    reportLinks :: HashMap Text [Text]
  }

instance Semigroup Report where
  Report a b c d e <> Report a' b' c' d' e' =
    Report (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Monoid Report where
  mempty = Report mempty mempty mempty mempty mempty

data ActionNode = ActionNode Text [ActionNode]

--------------------------------------------------------------------------------

generateReport :: ESPM board board' () -> Text
generateReport prog = T.unlines $ intercalate [""] blocks
  where
    blocks = catMaybes [compBlock, pinBlock, automationBlock]
    acc = execWriter $ toReportWriter prog
    scripts = HashMap.fromList [(scriptName s, scriptAction s) | s <- reportScripts acc]
    comps = reportComponents acc
    pins = sortOn (\p -> (pinType p, pinNumber p)) (reportPins acc)
    automationReports = reportAutomations acc
    compBlock = do
      guard $ not $ null comps
      let links = reportLinks acc
      Just $ "Components" : T.replicate (T.length "Components") "─" : do
        let maxName = foldl' max 0 (map (T.length . componentName) comps)
        c <- comps
        let suffix = case HashMap.lookup (componentName c) links of
              Just os | not (null os) -> "   ⟶ " <> T.intercalate ", " os
              _ -> ""
        pure
          $ pad (maxName + 4) (componentName c)
          <> componentKind c
          <> maybe "" (" " <>) (componentPlatform c)
          <> suffix

    pinBlock = do
      guard $ not $ null pins
      Just $ "Pins" : T.replicate (T.length "Pins") "─" : do
        p <- pins
        pure
          $ pad (maxPin + 5) (pinType p <> " " <> show (pinNumber p))
          <> pinName p
      where
        maxPin = foldl' max 0 $ do
          p <- pins
          [T.length (pinType p <> " " <> show (pinNumber p))]

    automationBlock = do
      guard $ not $ null automationReports
      Just $ "Automations" : T.replicate (T.length "Automations") "─" : do
        concatMap renderAutomation automationReports

    renderAutomation (AutomationReport src evt act) =
      (src <> "." <> evt) : concatMap (renderNode 1) (actionNodes scripts act)

    renderNode lvl (ActionNode label children) =
      let spaces = 2 + 5 * (lvl - 1)
       in T.replicate spaces " " <> "└── " <> label
            : concatMap (renderNode (lvl + 1)) children

--------------------------------------------------------------------------------

toReportWriter :: ESPM board board' () -> Writer Report ()
toReportWriter = ifoldFree reportAlg
  where
    reportAlg :: forall i j. ESPF i j (Writer Report ()) -> Writer Report ()
    reportAlg (MkESPHome _ next) = next
    reportAlg (MkBoard next) = next
    reportAlg (MkLogger next) = next
    reportAlg (MkWifi _ next) = next
    reportAlg (MkAPI _ next) = next
    reportAlg (MkOTA _ next) = next
    reportAlg (MkWebServer _ next) = next
    reportAlg (MkScript @name action next) = do
      let scriptReport = ScriptReport (T.pack (snakeCase (symbolVal (Proxy @name)))) action
      tell $ Report [] [] [scriptReport] [] mempty
      next
    reportAlg (MkOutput @name @platform @pin _opts next) = do
      let n = T.pack (symbolVal (Proxy @name))
          sym = T.pack (symbolVal (Proxy @(PlatformToSymbol platform)))
          pinType = if sym == "ledc" then "LEDC" else "GPIO"
          componentReport = ComponentReport n "Output" (Just (prettyPlatform sym))
          pinReport = PinReport pinType (fromIntegral (natVal (Proxy @pin))) n
      tell $ Report [componentReport] [pinReport] [] [] mempty
      next
    reportAlg (MkLight @name @platform _options platformOptions next) = do
      let n = T.pack (symbolVal (Proxy @name))
          sym = T.pack (symbolVal (Proxy @(PlatformToSymbol platform)))
          outs = HashMap.singleton n (extractLinks (toKeyMap platformOptions))
          componentReport =
            ComponentReport n "Light" (Just (prettyPlatform sym))
      tell $ Report [componentReport] [] [] [] outs
      next
    reportAlg (MkBinarySensor @name @platform @pin options _platformOptions next) = do
      let n = T.pack (symbolVal (Proxy @name))
          sym = T.pack (symbolVal (Proxy @(PlatformToSymbol platform)))
          componentReport = ComponentReport n "Binary Sensor" $ Just (prettyPlatform sym)
          pinReport = PinReport "GPIO" (fromIntegral (natVal (Proxy @pin))) n
          automationReports =
            catMaybes
              [ mkAuto n "onPress" (onPress options),
                mkAuto n "onRelease" (onRelease options),
                mkAuto n "onClick" (onClick options),
                mkAuto n "onDoubleClick" (onDoubleClick options),
                mkAuto n "onLongPress" (onLongPress options)
              ]
      tell $ Report [componentReport] [pinReport] [] automationReports mempty
      next
    reportAlg (MkSwitch @name @platform @pin opts next) = do
      let n = T.pack (symbolVal (Proxy @name))
          sym = T.pack (symbolVal (Proxy @(PlatformToSymbol platform)))
          componentReport = ComponentReport n "Switch" (Just (prettyPlatform sym))
          pinReport = PinReport "GPIO" (fromIntegral (natVal (Proxy @pin))) n
          automationReports =
            catMaybes
              [ mkAuto n "onTurnOn" (onTurnOn opts),
                mkAuto n "onTurnOff" (onTurnOff opts)
              ]
      tell $ Report [componentReport] [pinReport] [] automationReports mempty
      next
    reportAlg (MkButton @name @pin options next) = do
      let n = T.pack (symbolVal (Proxy @name))
          componentReport = ComponentReport n "Button" Nothing
          pinReport = PinReport "GPIO" (fromIntegral (natVal (Proxy @pin))) n
          automationReports =
            catMaybes [mkAuto n "onPress" (buttonOnPress options)]
      tell $ Report [componentReport] [pinReport] [] automationReports mempty
      next
    reportAlg (MkSensor @name @platform @pin _opts _platformOpts next) = do
      let n = T.pack (symbolVal (Proxy @name))
          sym = T.pack (symbolVal (Proxy @(PlatformToSymbol platform)))
          platform = prettyPlatform sym
          componentReport = ComponentReport n "Sensor" (Just platform)
          pinReport = PinReport "ADC" (fromIntegral (natVal (Proxy @pin))) n
      tell $ Report [componentReport] [pinReport] [] [] mempty
      next
    reportAlg (MkTextSensor @name next) = do
      let n = T.pack (symbolVal (Proxy @name))
      tell $ Report [ComponentReport n "Text Sensor" Nothing] [] [] [] mempty
      next
    reportAlg (MkNumber @name _options next) = do
      let n = T.pack (symbolVal (Proxy @name))
      tell $ Report [ComponentReport n "Number" Nothing] [] [] [] mempty
      next
    reportAlg (MkSelect @name _options next) = do
      let n = T.pack (symbolVal (Proxy @name))
      tell $ Report [ComponentReport n "Select" Nothing] [] [] [] mempty
      next
    reportAlg (MkCover @name @platform opts next) = do
      let n = T.pack (symbolVal (Proxy @name))
          sym = T.pack (symbolVal (Proxy @(PlatformToSymbol platform)))
          outs = HashMap.singleton n (extractLinks (toKeyMap opts))
          component = ComponentReport n "Cover" (Just (prettyPlatform sym))
      tell $ Report [component] [] [] [] outs
      next
    reportAlg (MkI2C @name _options next) = do
      let n = T.pack (symbolVal (Proxy @name))
      let componentReport = ComponentReport n "I2C" Nothing
      tell $ Report [componentReport] [] [] [] mempty
      next
    reportAlg (MkPN532I2C @name options next) = do
      let n = T.pack (symbolVal (Proxy @name))
      let automations = catMaybes [mkAuto n "onTag" =<< pn532I2COnTag options]
      let component = ComponentReport n "PN532 I2C" Nothing
      tell $ Report [component] [] [] automations mempty
      next
    reportAlg (MkInterval @name options next) = do
      let n = T.pack (symbolVal (Proxy @name))
      let automations = catMaybes [mkAuto n "interval" (intervalAction options)]
      let component = ComponentReport n "Interval" Nothing
      tell $ Report [component] [] [] automations mempty
      next

--------------------------------------------------------------------------------

mkAuto :: Text -> Text -> ESPAction () -> Maybe AutomationReport
mkAuto source event action
  | isNoAction action = Nothing
  | otherwise =
      Just
        AutomationReport
          { automationSource = source,
            automationEvent = event,
            automationAction = action
          }

isNoAction :: ESPAction () -> Bool
isNoAction (Pure _) = True
isNoAction _ = False

prettyPlatform :: Text -> Text
prettyPlatform "gpio" = "GPIO"
prettyPlatform "ledc" = "LEDC"
prettyPlatform "monochromatic" = "Monochromatic"
prettyPlatform "adc" = "ADC"
prettyPlatform "rgb" = "RGB"
prettyPlatform "cwww" = "CWWW"
prettyPlatform "endstop" = "Endstop"
prettyPlatform "template" = "Template"
prettyPlatform s = s

pad :: Int -> Text -> Text
pad w t = t <> T.replicate (max 0 (w - T.length t)) " "

linkKeys :: [Text]
linkKeys =
  mconcat
    [ ["output"],
      ["red", "green", "blue"],
      ["cold_white", "warm_white"],
      ["open_endstop", "close_endstop"]
    ]

extractLinks :: KeyMap Value -> [Text]
extractLinks km =
  catMaybes
    [ case v of String t -> Just (T.pack (snakeCase (T.unpack t))); _ -> Nothing
    | (k, v) <- KM.toList km,
      AK.toText k `elem` linkKeys
    ]

actionNodes :: HashMap Text (ESPAction ()) -> ESPAction () -> [ActionNode]
actionNodes _ (Pure ()) = []
actionNodes scripts (Free fx) = actionNode continued
  where
    continued = actionNodes scripts <$> fx
    label :: forall (n :: Symbol). (KnownSymbol n) => Proxy n -> Text
    label p = T.pack (snakeCase (symbolVal p))
    actionNode (CloseCover @name _ children) =
      [ActionNode ("close " <> label (Proxy @name)) []] <> children
    actionNode (DecrementNumber @name _ val children) =
      let name = "decrement " <> label (Proxy @name) <> " by " <> show val
       in [ActionNode name []] <> children
    actionNode (Delay ms children) =
      [ActionNode ("delay " <> show ms <> "ms") []] <> children
    actionNode (ComponentUpdate cID children) =
      [ActionNode ("update " <> cID) []] <> children
    actionNode (ComponentSuspend cID children) =
      [ActionNode ("suspend " <> cID) []] <> children
    actionNode (IfSwitchIsOn @name _ thenA elseA children) =
      let name = label (Proxy @name) <> " is_on"
          subNodes =
            actionNodes scripts thenA <> maybe [] (actionNodes scripts) elseA
       in [ActionNode name subNodes] <> children
    actionNode (IfSwitchIsOff @name _ thenA elseA children) =
      let name = label (Proxy @name) <> " is_off"
          subNodes =
            actionNodes scripts thenA <> maybe [] (actionNodes scripts) elseA
       in [ActionNode name subNodes] <> children
    actionNode (IncrementNumber @name _ val children) =
      let name = unwords ["increment", label (Proxy @name), "by", show val]
       in [ActionNode name []] <> children
    actionNode (LogMsg msg children) =
      [ActionNode ("log: " <> msg) []] <> children
    actionNode (OpenCover @name _ children) =
      [ActionNode ("open " <> label (Proxy @name)) []] <> children
    actionNode (RunScript @name _script children) =
      let nm = "run " <> label (Proxy @name)
          mScript = HashMap.lookup (label (Proxy @name)) scripts
          nodes = maybe [] (actionNodes scripts) mScript
       in [ActionNode nm nodes] <> children
    actionNode (SampleSensor @name _ children) =
      [ActionNode ("sample " <> label (Proxy @name)) []] <> children
    actionNode (SampleTextSensor @name _ children) =
      [ActionNode ("sample " <> label (Proxy @name)) []] <> children
    actionNode (SetNumber @name _ val children) =
      let name = "set " <> label (Proxy @name) <> " to " <> show val
       in [ActionNode name []] <> children
    actionNode (SetOutputValue @name _ val children) =
      let name = "set " <> label (Proxy @name) <> " level " <> show val
       in [ActionNode name []] <> children
    actionNode (StopCover @name _ children) =
      [ActionNode ("stop " <> label (Proxy @name)) []] <> children
    actionNode (ToggleSwitch @name _ children) =
      [ActionNode ("toggle " <> label (Proxy @name)) []] <> children
    actionNode (TurnOffLight @name _ children) =
      [ActionNode ("turn off " <> label (Proxy @name)) []] <> children
    actionNode (TurnOffOutput @name _ children) =
      [ActionNode ("turn off output " <> label (Proxy @name)) []] <> children
    actionNode (TurnOffSwitch @name _ children) =
      [ActionNode ("turn off switch " <> label (Proxy @name)) []] <> children
    actionNode (TurnOnLight @name _ children) =
      [ActionNode ("turn on " <> label (Proxy @name)) []] <> children
    actionNode (TurnOnOutput @name _ children) =
      [ActionNode ("turn on output " <> label (Proxy @name)) []] <> children
    actionNode (TurnOnSwitch @name _ children) =
      [ActionNode ("turn on switch " <> label (Proxy @name)) []] <> children
