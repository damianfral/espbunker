{-# LANGUAGE BlockArguments #-}
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
import ESPBunker.DeviceClass (DeviceClass (..))
import ESPBunker.Language
import GHC.TypeLits (KnownSymbol)
import Relude hiding ((>>=))

-- | A common setup for all the examples.
commonSetup :: _
commonSetup = do
  _ <- board @ESP32C3
  _ <- esphome @"esphome-bunker" def
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
  _ <- binarySensor @"btn1" @GPIO @1 def {onPress = blinkScript} Nothing
  done

coverExample :: ESPM ESP32C3 _ ()
coverExample = do
  _ <- commonSetup
  openEndstopSensor <- binarySensor @"open_endstop" @GPIO @4 def Nothing
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
  _ <- switch @"switch_with_restore" @GPIO @0 def {switchRestoreMode = Just ALWAYS_ON}
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

sensorWithOptionsExample :: ESPM ESP32C3 _ ()
sensorWithOptionsExample = do
  _ <- commonSetup
  _ <-
    sensor @"temperature_sensor" @ADC @3
      def
        { sensorUnit = "°C",
          sensorAccuracy = Just 2,
          sensorIntervalMs = Just 10000,
          sensorStateClass = Just STATE_CLASS_MEASUREMENT,
          sensorDeviceClass = Just DeviceClassTemperature,
          sensorIcon = Just "mdi:thermometer",
          sensorEntityCategory = Just "diagnostic",
          sensorInternal = Just False
        }
      (SensorADCOptions $ Just ATTEN_11DB)
  done

binarySensorWithOptionsExample :: ESPM ESP32C3 _ ()
binarySensorWithOptionsExample = do
  _ <- commonSetup
  let onMotion = log "Motion detected!"
  _ <-
    binarySensor @"motion_sensor" @GPIO @2
      def
        { onPress = onMotion,
          binarySensorDeviceClass = Nothing, -- Motion is not a valid device class for gpio binary sensors
          binarySensorIcon = Just "mdi:motion-sensor",
          binarySensorEntityCategory = Just "diagnostic",
          binarySensorInternal = Just False
        }
      def
  done

lightWithOptionsExample :: ESPM ESP32C3 _ ()
lightWithOptionsExample = do
  _ <- commonSetup
  out1 <- output @"light_out1" @LEDC @2 def
  _ <-
    light @"enhanced_light" @Monochromatic
      def
        { lightTransitionLength = Just 1000,
          lightEffects = ["pulse", "random"],
          lightColorMode = Just COLOR_MODE_BRIGHTNESS,
          lightGammaCorrect = Just 2.8,
          lightDefaultTransitionLength = Just 2000,
          lightDeviceClass = Just DeviceClassLight,
          lightIcon = Just "mdi:lightbulb",
          lightEntityCategory = Just "config",
          lightInternal = Just False,
          lightRestoreMode = Just RESTORE_DEFAULT_OFF
        }
      $ LightMonochromaticOptions out1
  done

switchWithOptionsExample :: ESPM ESP32C3 _ ()
switchWithOptionsExample = do
  _ <- commonSetup
  _ <-
    switch @"enhanced_switch" @GPIO @0
      def
        { switchRestoreMode = Just ALWAYS_ON,
          switchDeviceClass = Just DeviceClassOutlet,
          switchIcon = Just "mdi:power-plug",
          switchEntityCategory = Just "config",
          switchInternal = Just False
        }
  done

coverWithOptionsExample :: ESPM ESP32C3 _ ()
coverWithOptionsExample = do
  _ <- commonSetup
  _ <-
    cover @"template_cover" @Template
      def
        { coverDeviceClass = Nothing, -- "shutter" might not be valid for template covers, use no device class
          coverIcon = Just "mdi:window-shutter",
          coverEntityCategory = Just "config",
          coverInternal = Just False,
          coverAssumedState = Just True,
          coverOptimistic = Just False
        }
  done

numberWithOptionsExample :: ESPM ESP32C3 _ ()
numberWithOptionsExample = do
  _ <- commonSetup
  _ <-
    number @"brightness_control"
      def
        { numberMin = Just 0.0,
          numberMax = Just 100.0,
          numberStep = Just 5.0,
          numberUnit = Just "%",
          numberDeviceClass = Nothing, -- "brightness" is not valid for number components
          numberIcon = Just "mdi:brightness-5",
          numberEntityCategory = Just "config",
          numberInternal = Just False,
          numberMode = Just "slider",
          numberOptimistic = Just True
        }
  done

-- Christmas lights and NFC example
christmasExample :: ESPM ESP32C3 _ ()
christmasExample = do
  _ <- board @ESP32C3
  _ <-
    esphome @"christmas" $ def & addBootAction (Just (-100)) do
      log "Booted"
      delay 2000
  _ <- logger
  _ <-
    wifi
      $ def
      & addNetwork "GL-AR300M-b66" "!secret password-GL-AR300M-b66"
      & ap "christmas-hotspot" "!secret password-ap"
  _ <- api "!secret password-api"
  _ <- ota [OTAOptions "esphome" "!secret password-ota"]
  _ <- webServer 80

  -- Relays
  relay2 <- switch @"relay_2" @GPIO @19 def {switchInverted = Just True}
  relay3 <- switch @"relay_3" @GPIO @4 def {switchInverted = Just True}
  relay4 <- switch @"relay_4" @GPIO @5 def {switchInverted = Just True}
  relay5 <- switch @"relay_5" @GPIO @6 def {switchInverted = Just True}
  relay6 <- switch @"relay_6" @GPIO @7 def {switchInverted = Just True}
  relay7 <- switch @"relay_7" @GPIO @20 def {switchInverted = Just True}

  -- I2C configuration for NFC
  _ <-
    i2c
      $ def
        { i2cSda = "GPIO2",
          i2cScl = "GPIO3",
          i2cScan = Just True
        }

  -- Placeholder: PN532 I2C component (I2C is a configuration component without a return value)
  -- Note: Since I don't have an action to reference the NFC reader directly, I'll just set up the I2C
  -- For demonstration purposes only, this would connect to something that can be updated/suspended

  -- Scripts for relay sequence
  relaySequenceOnRightScript <- script @"relay_sequence_on_right" $ do
    turnOn relay2
    delay 50
    turnOn relay3
    delay 50
    turnOn relay4
    delay 50
    turnOn relay5
    delay 50
    turnOn relay6
    delay 50
    log "All LED relays are ON."

  relaySequenceOnLeftScript <- script @"relay_sequence_on_left" $ do
    turnOn relay6
    delay 50
    turnOn relay5
    delay 50
    turnOn relay4
    delay 50
    turnOn relay3
    delay 50
    turnOn relay2
    delay 50
    log "All LED relays are ON."

  relaySequenceOffLeftScript <- script @"relay_sequence_off_left" $ do
    turnOff relay6
    delay 50
    turnOff relay5
    delay 50
    turnOff relay4
    delay 50
    turnOff relay3
    delay 50
    turnOff relay2
    delay 50
    log "All LED relays are OFF."

  relaySequenceOffRightScript <- script @"relay_sequence_off_right" $ do
    turnOff relay2
    delay 50
    turnOff relay3
    delay 50
    turnOff relay4
    delay 50
    turnOff relay5
    delay 50
    turnOff relay6
    delay 50
    log "All LED relays are OFF."

  relaySequenceOffScript <- script @"relay_sequence_off" $ do
    turnOff relay6
    turnOff relay5
    turnOff relay4
    turnOff relay3
    turnOff relay2

  -- NFC scripts
  nfcOnScript <- script @"nfc_on" $ do
    log "Turning on NFC..."
    turnOn relay7
    -- Just log instead of trying to update non-existent nfc_reader component
    log "NFC turned on"

  nfcToggleScript <- script @"nfc_toggle" $ do
    -- Using if condition to check switch state - need to implement this properly
    -- For now, just call the respective scripts
    log "NFC toggle script executed"

  -- Main relay sequence script (now that nfcOnScript is defined)
  relaySequenceScript <- script @"relay_sequence" $ do
    log "Starting relay activation sequence..."

    runScript relaySequenceOnRightScript
    delay 250

    runScript relaySequenceOffRightScript
    delay 250

    runScript relaySequenceOnLeftScript
    delay 250

    runScript relaySequenceOffLeftScript
    delay 250

    runScript relaySequenceOnRightScript
    delay 250

    runScript relaySequenceOffLeftScript
    delay 250

    runScript relaySequenceOnRightScript
    delay 250

    runScript relaySequenceOffRightScript
    delay 250

    runScript relaySequenceOnLeftScript
    delay 250

    runScript relaySequenceOffLeftScript
    delay 250

    runScript relaySequenceOnRightScript
    delay 250

    runScript relaySequenceOffRightScript
    delay 250

    runScript relaySequenceOnLeftScript
    delay 250

    runScript relaySequenceOffRightScript
    delay 250

    runScript relaySequenceOnLeftScript
    delay 250

    log "All LED relays are ON."
    delay 25000

    runScript relaySequenceOffScript
    log "All LED relays are OFF."
    runScript nfcOnScript

  -- Binary sensors for buttons with pin modes
  _ <-
    binarySensor @"nfc_toggle_button" @GPIO @9
      def {onPress = runScript nfcToggleScript, binarySensorPinMode = Just $ PinMode {pinModeInput = True, pinModeOutput = False, pinModeOpenDrain = False, pinModePullUp = True, pinModePullDown = False}}
      def

  _ <-
    binarySensor @"run_leds" @GPIO @8
      def {onPress = runScript relaySequenceScript, binarySensorPinMode = Just $ PinMode {pinModeInput = True, pinModeOutput = False, pinModeOpenDrain = False, pinModePullUp = True, pinModePullDown = False}}
      def

  -- Interval component (just log to demonstrate the functionality)
  _ <-
    interval
      $ def
        { intervalId = Nothing, -- Use default ID
          intervalInterval = Just "10s",
          intervalAction = log "Running periodic interval task"
        }
  done

priorityExample :: ESPM ESP32C3 _ ()
priorityExample = do
  _ <- board @ESP32C3
  _ <- esphome @"priority-test" $ addBootAction (Just 500) (log "Starting up") def
  _ <- logger
  _ <- wifi $ def & addNetwork "test-ssid" "test-password"
  _ <- ota [OTAOptions "esphome" "pass"]
  _ <- webServer 80
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
    ("switchlightOutputExample", SomeESPM switchlightOutputExample),
    ("sensor with options", SomeESPM sensorWithOptionsExample),
    ("binary sensor with options", SomeESPM binarySensorWithOptionsExample),
    ("light with options", SomeESPM lightWithOptionsExample),
    ("switch with options", SomeESPM switchWithOptionsExample),
    ("cover with options", SomeESPM coverWithOptionsExample),
    ("number with options", SomeESPM numberWithOptionsExample),
    ("christmas example", SomeESPM christmasExample),
    ("priority example", SomeESPM priorityExample)
  ]
