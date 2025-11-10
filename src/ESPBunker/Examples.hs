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
  r1 <- switch @"switch1" @0
  r <- output @"r" @2
  g <- output @"g" @3
  b <- output @"b" @4
  let blinkScript = do
        turnOn r1
        delay 1000
        turnOff r1
  _ <- binarySensor @"btn1" @GPIO @1 def {onPress = blinkScript} def
  _ <- light @"light1" @RGB def $ LightRGBOptions {red = r, green = g, blue = b}
  done
