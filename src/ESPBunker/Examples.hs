{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module ESPBunker.Examples where

import Data.Default
import ESPBunker
import ESPBunker.DeviceClass (DeviceClass (..))
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
  ledOut <- output @"led_out" @LEDC @2 def
  lamp <- light @"lamp" @Monochromatic def $ LightMonochromaticOptions ledOut
  let blinkScript = replicateM_ 4 $ do
        turnOnL lamp
        delay 300
        turnOffL lamp
        delay 300
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
  monoLight <- light @"mono_light" @Monochromatic def $ LightMonochromaticOptions out1

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

  -- Physical switch controls the monochromatic light
  _ <-
    switch @"mono_switch" @GPIO @0
      def
        { onTurnOn = turnOnL monoLight,
          onTurnOff = turnOffL monoLight
        }
  done

outputExample :: ESPM ESP32C3 _ ()
outputExample = do
  _ <- commonSetup
  out <- output @"ledc_with_freq" @LEDC @2 def {frequency = Just 25000}
  lamp <- light @"lamp" @Monochromatic def $ LightMonochromaticOptions out
  let onAction = do
        log "Lamp ON"
        turnOnL lamp
      offAction = do
        log "Lamp OFF"
        turnOffL lamp
  _ <-
    switch @"light_switch" @GPIO @0
      def
        { onTurnOn = onAction,
          onTurnOff = offAction
        }
  done

sensorExample :: ESPM ESP32C3 _ ()
sensorExample = do
  _ <- commonSetup
  _ <- sensor @"adc_sensor" @ADC @3 def (SensorADCOptions (Just ATTEN_11DB))
  done

switchExample :: ESPM ESP32C3 _ ()
switchExample = do
  _ <- commonSetup
  out <- output @"lamp_out" @LEDC @2 def
  lamp <- light @"lamp" @Monochromatic def $ LightMonochromaticOptions out
  let onAction = do
        log "Switch ON — lighting lamp"
        turnOnL lamp
      offAction = do
        log "Switch OFF — extinguishing lamp"
        turnOffL lamp
  _ <-
    switch @"switch_with_restore" @GPIO @0
      def
        { switchRestoreMode = Just ALWAYS_ON,
          onTurnOn = onAction,
          onTurnOff = offAction
        }
  done

data SomeESPM where
  SomeESPM ::
    forall
      board
      boardName
      names
      gpioPins
      adcPins
      ledcPins
      board'
      boardName'
      names'
      gpioPins'
      adcPins'
      ledcPins'.
    ( board ~ Board boardName names gpioPins adcPins ledcPins,
      board' ~ Board boardName' names' gpioPins' adcPins' ledcPins',
      KnownSymbol boardName,
      KnownSymbol boardName'
    ) =>
    ESPM board board' () -> SomeESPM

scriptExample :: ESPM ESP32C3 _ ()
scriptExample = do
  _ <- commonSetup
  out <- output @"lamp_out" @LEDC @2 def
  lamp <- light @"lamp" @Monochromatic def $ LightMonochromaticOptions out
  toggleScript <- script @"toggle_script" $ do
    log "Toggling lamp via script"
    turnOnL lamp
    delay 1000
    turnOffL lamp
    delay 500
    turnOnL lamp
  _ <- binarySensor @"btn1" @GPIO @1 def {onPress = runScript toggleScript} def
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
  out <- output @"light_out" @LEDC @0 def
  lamp <- light @"motion_lamp" @Monochromatic def $ LightMonochromaticOptions out
  let onMotion = do
        log "Motion detected! Turning on light for 10s"
        turnOnL lamp
        delay 10000
        turnOffL lamp
  _ <-
    binarySensor @"motion_sensor" @GPIO @2
      def
        { onPress = onMotion,
          binarySensorDeviceClass = Nothing,
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
  enhancedLight <-
    light @"enhanced_light" @Monochromatic
      def
        { lightEffects = ["pulse", "random"],
          lightGammaCorrect = Just 2.8,
          lightDefaultTransitionLength = Just 2000,
          lightIcon = Just "mdi:lightbulb",
          lightEntityCategory = Just "config",
          lightInternal = Just False,
          lightRestoreMode = Just RESTORE_DEFAULT_OFF
        }
      $ LightMonochromaticOptions out1
  _ <-
    switch @"light_switch" @GPIO @0
      def
        { onTurnOn = turnOnL enhancedLight,
          onTurnOff = turnOffL enhancedLight
        }
  done

switchWithOptionsExample :: ESPM ESP32C3 _ ()
switchWithOptionsExample = do
  _ <- commonSetup
  out <- output @"lamp_out" @LEDC @2 def
  lamp <- light @"lamp" @Monochromatic def $ LightMonochromaticOptions out
  let onAction = do
        log "Outlet ON — lamp lit"
        turnOnL lamp
      offAction = do
        log "Outlet OFF — lamp off"
        turnOffL lamp
  _ <-
    switch @"enhanced_switch" @GPIO @0
      def
        { switchRestoreMode = Just ALWAYS_ON,
          onTurnOn = onAction,
          onTurnOff = offAction,
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

selectExample :: ESPM ESP32C3 _ ()
selectExample = do
  _ <- commonSetup
  _ <-
    select @"mode_selector"
      def
        { selectOptions = ["eco", "comfort", "boost"],
          selectInitialOption = Just "eco",
          selectIcon = Just "mdi:thermostat",
          selectEntityCategory = Just "config",
          selectOptimistic = Just True
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
  _ <- api $ EncryptionKey "0123456789abcdef0123456789abcdef"
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
  _ <- i2c @"nfc_i2c" $ def {i2cSda = "GPIO2", i2cScl = "GPIO3", i2cScan = Just True}

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
      def
        { onPress = runScript nfcToggleScript,
          binarySensorPinMode =
            Just
              $ PinMode
                { pinModeInput = True,
                  pinModeOutput = False,
                  pinModeOpenDrain = False,
                  pinModePullUp = True,
                  pinModePullDown = False
                }
        }
      def

  _ <-
    binarySensor @"run_leds" @GPIO @8
      def
        { onPress = runScript relaySequenceScript,
          binarySensorPinMode =
            Just
              $ PinMode
                { pinModeInput = True,
                  pinModeOutput = False,
                  pinModeOpenDrain = False,
                  pinModePullUp = True,
                  pinModePullDown = False
                }
        }
      def

  -- Interval component (just log to demonstrate the functionality)
  _ <-
    interval @"keepalive"
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

buttonExample :: ESPM ESP32C3 _ ()
buttonExample = do
  _ <- board @ESP32C3
  _ <- esphome @"button-test" def
  _ <- logger
  _ <- wifi $ def & addNetwork "ssid" "password"
  relay <- switch @"power_relay" @GPIO @18 def
  let powerCycle = do
        log "Power cycling system..."
        turnOff relay
        delay 2000
        turnOn relay
        log "Power cycle complete"
  _ <-
    button @"reset_button" @19
      def
        { onPress = powerCycle,
          binarySensorIcon = Just "mdi:restart",
          binarySensorEntityCategory = Just "config"
        }
  _ <- ota [OTAOptions "esphome" "pass"]
  _ <- webServer 80
  done

apiExample :: ESPM ESP32C3 _ ()
apiExample = do
  _ <- board @ESP32C3
  _ <- esphome @"api-test" def
  _ <- logger
  _ <- wifi $ def & addNetwork "ssid" "password"
  _ <- api $ EncryptionKey "0123456789abcdef0123456789abcdef"
  _ <- ota [OTAOptions "esphome" "pass"]
  _ <- webServer 80
  done

otaMultipleExample :: ESPM ESP32C3 _ ()
otaMultipleExample = do
  _ <- board @ESP32C3
  _ <- esphome @"ota-multi-test" def
  _ <- logger
  _ <- wifi $ def & addNetwork "ssid" "password"
  _ <- ota [OTAOptions "esphome" "password"]
  _ <- webServer 80
  done

switchWithAllOptionsExample :: ESPM ESP32C3 _ ()
switchWithAllOptionsExample = do
  _ <- board @ESP32C3
  _ <- esphome @"switch-all-options" def
  _ <- logger
  _ <- wifi $ def & addNetwork "ssid" "password"
  _ <-
    switch @"full_featured_switch" @GPIO @10
      def
        { switchRestoreMode = Just ALWAYS_ON,
          onTurnOn = log "Switch turned ON",
          onTurnOff = log "Switch turned OFF",
          switchDeviceClass = Just DeviceClassOutlet,
          switchIcon = Just "mdi:toggle-switch",
          switchEntityCategory = Just "config",
          switchInternal = Just False,
          switchInterlock = [],
          switchInterlockWaitTime = Nothing,
          switchInverted = Just False
        }
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
    ("select", SomeESPM selectExample),
    ("christmas example", SomeESPM christmasExample),
    ("priority example", SomeESPM priorityExample),
    ("button", SomeESPM buttonExample),
    ("api", SomeESPM apiExample),
    ("ota multiple", SomeESPM otaMultipleExample),
    ("switch with all options", SomeESPM switchWithAllOptionsExample)
  ]
