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
commonSetup = do
  _ <- board @ESP32C3
  _ <- esphome @"esphome-bunker"
  _ <- logger
  _ <-
    wifi
      $ def
      & addNetwork "my-ssid" "my-password"
      & ap "my-ap-ssid" "my-ap-password"
  _ <- webServer 80
  _ <- ota [OTAOptions "esphome" "pass"]
  done

binarySensorExample :: ESPM ESP32C3 _ ()
binarySensorExample = do
  _ <- commonSetup
  let blinkScript = replicateM_ 4 $ do
        log "Blinking..."
        delay 1000
  _ <- binarySensor @"btn1" @GPIO @1 def {onPress = blinkScript} def
  done

coverExample :: ESPM ESP32C3 _ ()
coverExample = do
  _ <- commonSetup
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
lightExample = do
  _ <- commonSetup
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
outputExample = do
  _ <- commonSetup
  _ <- output @"ledc_with_freq" @LEDC @2 def {frequency = Just 25000}
  done

sensorExample :: ESPM ESP32C3 _ ()
sensorExample = do
  _ <- commonSetup
  _ <- sensor @"adc_sensor" @ADC @3 def (SensorADCOptions (Just ATTEN_11DB))
  done

switchExample :: ESPM ESP32C3 _ ()
switchExample = do
  _ <- commonSetup
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

scriptExample :: ESPM ESP32C3 _ ()
scriptExample = do
  _ <- commonSetup
  logScript <- script @"script1" $ log "asd"
  _ <- binarySensor @"btn1" @GPIO @1 def {onPress = runScript logScript} def
  done

switchlightOutputExample :: ESPM ESP32C3 _ ()
switchlightOutputExample = do
  _ <- commonSetup
  out1 <- output @"out1" @LEDC @2 def -- TODO: check the platforms
  l <- light @"light_out" @Monochromatic def $ LightMonochromaticOptions out1
  _ <-
    switch @"light_switch" @GPIO @18
      def {onTurnOn = turnOnL l, onTurnOff = turnOffL l}
  done

examples :: [(Text, SomeESPM)]
examples =
  [ ("binary sensor", SomeESPM binarySensorExample),
    ("cover", SomeESPM coverExample),
    ("light", SomeESPM lightExample),
    ("output", SomeESPM outputExample),
    ("sensor", SomeESPM sensorExample),
    ("switch", SomeESPM switchExample),
    ("script", SomeESPM scriptExample),
    ("switchlightOutputExample", SomeESPM switchlightOutputExample)
  ]
