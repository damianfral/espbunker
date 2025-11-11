{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module ESPBunker.Examples where

import Data.Default
import ESPBunker.Language
import GHC.TypeLits (KnownSymbol)
import Relude hiding ((>>=))

-- | A common setup for all the examples.
commonSetup :: _
commonSetup f = do
  _ <- board @ESP32C3
  _ <- esphome @"esphome-bunker"
  _ <- logger
  _ <-
    wifi
      $ def
      & addNetwork "my-ssid" "my-password"
      & ap "my-ap-ssid" "my-ap-password"
  -- _ <- webServer 80
  _ <- ota [OTAOptions "esphome" "pass"]
  f done

binarySensorExample :: ESPM ESP32C3 _ ()
binarySensorExample = commonSetup $ \_ -> do
  let blinkScript = replicateM_ 4 $ do
        log "Blinking..."
        delay 1000
  _ <- binarySensor @"btn1" @GPIO @1 def {onPress = blinkScript} def
  done

coverExample :: ESPM ESP32C3 _ ()
coverExample = commonSetup $ \_ -> do
  openEndstopSensor <- binarySensor @"open_endstop" @GPIO @4 def def
  closeEndstopSensor <- binarySensor @"close_endstop" @GPIO @5 def def
  _ <-
    cover @"endstop_cover" @Endstop
      CoverEndstopOptions
        { openAction = log "Cover opening...",
          closeAction = log "Cover closing...",
          stopAction = log "Cover stopped.",
          openEndstop = openEndstopSensor,
          closeEndstop = closeEndstopSensor,
          openDuration = 30,
          closeDuration = 30
        }
  done

lightExample :: ESPM ESP32C3 _ ()
lightExample = commonSetup $ \_ -> do
  -- Monochromatic light
  out1 <- output @"out1" @LEDC @2 def
  _ <- light @"mono_light" @Monochromatic def $ LightMonochromaticOptions out1

  -- CWWW light
  cOut <- output @"c_out" @LEDC @3 def
  wOut <- output @"w_out" @LEDC @4 def
  _ <-
    light @"cwww_light" @CWWW def
      $ LightCWWWOptions
        { coldWhite = cOut,
          warmWhite = wOut,
          coldWhiteColorTemperature = "6500 K",
          warmWhiteColorTemperature = "2700 K"
        }

  -- RGB light
  r <- output @"r" @LEDC @5 def
  g <- output @"g" @LEDC @6 def
  b <- output @"b" @LEDC @7 def
  _ <- light @"rgb_light" @RGB def $ LightRGBOptions {red = r, green = g, blue = b}
  done

outputExample :: ESPM ESP32C3 _ ()
outputExample = commonSetup $ \_ -> do
  _ <- output @"ledc_with_freq" @LEDC @2 def {frequency = Just 25000}
  done

sensorExample :: ESPM ESP32C3 _ ()
sensorExample = commonSetup $ \_ -> do
  _ <- sensor @"adc_sensor" @ADC @3 def (SensorADCOptions (Just ATTEN_11DB))
  done

switchExample :: ESPM ESP32C3 _ ()
switchExample = commonSetup $ \_ -> do
  _ <- switch @"switch_with_restore" @GPIO @0 def {restoreMode = Just ALWAYS_ON}
  done

data SomeESPM where
  SomeESPM ::
    forall board boardName names pins board' boardName' names' pins'.
    ( board ~ Board boardName names pins,
      board' ~ Board boardName' names' pins',
      KnownSymbol boardName,
      KnownSymbol boardName'
    ) =>
    ESPM board board' () -> SomeESPM

examples :: [(Text, SomeESPM)]
examples =
  [ ("binary sensor", SomeESPM binarySensorExample),
    ("cover", SomeESPM coverExample),
    ("light", SomeESPM lightExample),
    ("output", SomeESPM outputExample),
    ("sensor", SomeESPM sensorExample),
    ("switch", SomeESPM switchExample)
  ]

-- binaryOutputExample :: ESPM ESP32C3 _ ()
-- binaryOutputExample = commonSetup $ \_ -> do
--   _ <- binaryOutput @"b_out" @8
--   done

-- buttonExample :: ESPM ESP32C3 _ ()
-- buttonExample = commonSetup $ \_ -> do
--   b_out <- binaryOutput @"b_out" @8
--   _ <- button @"btn2" @GPIO @9 def {onPress = toggle b_out} def
--   done

-- scriptExample :: ESPM ESP32C3 _ ()
-- scriptExample = commonSetup $ \_ -> do
--   num <- number @"my_number" def
--   _ <- script @"my_script" $ setNumber num 42
--   done

-- numberExample :: ESPM ESP32C3 _ ()
-- numberExample = commonSetup $ \_ -> do
--   _ <-
--     number @"my_number"
--       def
--         { numberMin = Just 0,
--           numberMax = Just 100,
--           numberStep = Just 2
--         }
--   done

-- selectExample :: ESPM ESP32C3 _ ()
-- selectExample = commonSetup $ \_ -> do
--   r <- output @"r" @LEDC @5 def
--   g <- output @"g" @LEDC @6 def
--   b <- output @"b" @LEDC @7 def
--   l <- light @"rgb_light" @RGB def {lightEffects = ["random", "strobe"]} $ LightRGBOptions {red = r, green = g, blue = b}
--   _ <-
--     select @"light_effect"
--       def
--         { selectOptions = ["none", "random", "strobe"]
--           -- onValue = script.execute "set_light_effect"
--         }
--   _ <- script @"set_light_effect" $ do
--     turnOnLWithEffect l "x"
--   done

-- outputGPIOExample :: ESPM ESP32C3 _ ()
-- outputGPIOExample = commonSetup $ \_ -> do
--   out <- output @"gpio_out" @GPIO @10 def
--   _ <- switch @"gpio_switch" @GPIO @18 def {onPress = turnOnO out}
--   done

-- lightOutExample :: ESPM ESP32C3 _ ()
-- lightOutExample = commonSetup $ \_ -> do
--   out <- output @"out" @GPIO @10 def
--   l <- light @"light_out" @Out def $ LightOutputOptions out
--   _ <- switch @"light_switch" @GPIO @18 def {onPress = turnOnL l, onRelease = turnOffL l}
--   done

-- allExamples :: [ESPM ESP32C3 _ ()]
-- allExamples =
-- [ binarySensorExample,
--   coverExample,
--   lightExample,
--   outputExample,
--   sensorExample,
--   switchExample
-- ]
