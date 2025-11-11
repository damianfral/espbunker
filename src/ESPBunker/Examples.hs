{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module ESPBunker.Examples where

import Data.Default
import ESPBunker.Language
import Relude hiding ((>>=))

example1 :: ESPM ESP32C3 _ ()
example1 = do
  -- We are forced to use explicit bindings due to the indexed (>>>=)
  _ <- board @ESP32C3
  _ <- esphome @"test"
  s <- switch @"switch1" @GPIO @0 def
  r <- output @"r" @LEDC @2 def
  g <- output @"g" @LEDC @3 def
  b <- output @"b" @LEDC @4 def
  let blinkScript = replicateM_ 4 $ do
        turnOn s
        delay 1000
        turnOff s
        delay 1000
  _ <- binarySensor @"btn1" @GPIO @1 def {onPress = blinkScript} def
  _ <- light @"light1" @RGB def $ LightRGBOptions {red = r, green = g, blue = b}
  done

example2 :: ESPM ESP32C3 _ ()
example2 = do
  _ <- board @ESP32C3
  _ <- esphome @"light_test"
  _pwmOut <- output @"led_pwm" @LEDC @5 def
  pwmR <- output @"mono_r" @LEDC @2 def
  pwmG <- output @"mono_g" @LEDC @3 def
  pwmB <- output @"mono_b" @LEDC @4 def
  _ <-
    light @"mono_light" @RGB def
      $ LightRGBOptions {red = pwmR, green = pwmG, blue = pwmB}
  done

example3 :: ESPM ESP32C3 _ ()
example3 = do
  _ <- board @ESP32C3
  _ <- esphome @"lights_comprehensive"
  out1 <- output @"out1" @LEDC @2 def
  _ <- light @"mono_light" @Monochromatic def $ LightMonochromaticOptions out1
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
  done

example4 :: ESPM ESP32C3 _ ()
example4 = do
  _ <- board @ESP32C3
  _ <- logger
  _ <- esphome @"new_features"
  _ <- switch @"switch_with_restore" @GPIO @0 def {restoreMode = Just ALWAYS_ON}
  _ <- output @"ledc_with_freq" @LEDC @2 def {frequency = Just 25000}
  _ <- sensor @"adc_sensor" @ADC @3 def (SensorADCOptions (Just ATTEN_11DB))
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
