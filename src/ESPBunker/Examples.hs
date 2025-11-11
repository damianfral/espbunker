{-# LANGUAGE DataKinds #-}
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
  s <- switch @"switch1" @GPIO @0
  r <- output @"r" @LEDC @2
  g <- output @"g" @LEDC @3
  b <- output @"b" @LEDC @4
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
  _pwmOut <- output @"led_pwm" @LEDC @5
  pwmR <- output @"mono_r" @LEDC @2
  pwmG <- output @"mono_g" @LEDC @3
  pwmB <- output @"mono_b" @LEDC @4
  _ <-
    light @"mono_light" @RGB def
      $ LightRGBOptions {red = pwmR, green = pwmG, blue = pwmB}
  done
