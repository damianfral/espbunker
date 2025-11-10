{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Examples where

import Data.Default
import ESPBunker.Language
import Relude hiding ((>>=))

example1 :: ESPM ESP32C3 (Board ["btn1", "switch1"] '[]) ()
example1 = do
  -- We are forced to use explicit bindings due to the indexed (>>>=)
  _ <- board @ESP32C3
  r1 <- switch @"switch1" @0
  _ <-
    binarySensor @"btn1" @GPIO @1
      def
        { onPress = do
            turnOn r1
            delay 1000
            turnOff r1
        }
      def
  done
