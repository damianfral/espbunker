{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.DeviceClass where

import Data.Aeson (ToJSON, toJSON)
import GHC.Generics (Generic)
import Relude (Eq, Show, Text)

data DeviceClass
  = DeviceClassBattery
  | DeviceClassConnectivity
  | DeviceClassDoor
  | DeviceClassGarageDoor
  | DeviceClassGas
  | DeviceClassHeat
  | DeviceClassLight
  | DeviceClassLock
  | DeviceClassMoisture
  | DeviceClassMotion
  | DeviceClassMoving
  | DeviceClassOccupancy
  | DeviceClassOpening
  | DeviceClassPlug
  | DeviceClassPower
  | DeviceClassPresence
  | DeviceClassProblem
  | DeviceClassSafety
  | DeviceClassSmoke
  | DeviceClassSound
  | DeviceClassVibration
  | DeviceClassWindow
  | DeviceClassWindowCovering
  | DeviceClassTemperature
  | DeviceClassHumidity
  | DeviceClassPressure
  | DeviceClassCurrent
  | DeviceClassEnergy
  | DeviceClassPowerFactor
  | DeviceClassFrequency
  | DeviceClassVoltage
  | DeviceClassSignalStrength
  | DeviceClassDataRate
  | DeviceClassDataSize
  | DeviceClassDistance
  | DeviceClassDuration
  | DeviceClassIlluminance
  | DeviceClassAccelG
  | DeviceClassAccelMs2
  | DeviceClassAltitude
  | DeviceClassArea
  | DeviceClassAqi
  | DeviceClassApparentPower
  | DeviceClassAtmosphericPressure
  | DeviceClassBaby
  | DeviceClassBaking
  | DeviceClassBeauty
  | DeviceClassBeverage
  | DeviceClassBoiler
  | DeviceClassBurglar
  | DeviceClassButton
  | DeviceClassCabinet
  | DeviceClassCarbonDioxide
  | DeviceClassCarbonMonoxide
  | DeviceClassCharger
  | DeviceClassCold
  | DeviceClassCommunication
  | DeviceClassConsumable
  | DeviceClassCooking
  | DeviceClassCounter
  | DeviceClassCurrentStage
  | DeviceClassDishwasher
  | DeviceClassDoorbell
  | DeviceClassDresser
  | DeviceClassDryer
  | DeviceClassEmergency
  | DeviceClassEmergencyCall
  | DeviceClassFan
  | DeviceClassFault
  | DeviceClassFilter
  | DeviceClassFire
  | DeviceClassFirmware
  | DeviceClassFlag
  | DeviceClassGarage
  | DeviceClassGasoline
  | DeviceClassGenerator
  | DeviceClassHvac
  | DeviceClassHazel
  | DeviceClassHealth
  | DeviceClassHeatCold
  | DeviceClassHelper
  | DeviceClassHolding
  | DeviceClassHood
  | DeviceClassHub
  | DeviceClassInverter
  | DeviceClassIrradiance
  | DeviceClassKeypad
  | DeviceClassLevel
  | DeviceClassLifeSafety
  | DeviceClassLighting
  | DeviceClassLinen
  | DeviceClassLockTight
  | DeviceClassMains
  | DeviceClassMedical
  | DeviceClassMode
  | DeviceClassMotionSensitivity
  | DeviceClassMovingRate
  | DeviceClassMower
  | DeviceClassMusic
  | DeviceClassNetwork
  | DeviceClassNight
  | DeviceClassNutrition
  | DeviceClassOil
  | DeviceClassOnline
  | DeviceClassOpeningLevel
  | DeviceClassOutlet
  | DeviceClassOxygen
  | DeviceClassPm1
  | DeviceClassPm10
  | DeviceClassPm25
  | DeviceClassParking
  | DeviceClassPeak
  | DeviceClassPerfume
  | DeviceClassPestControl
  | DeviceClassPh
  | DeviceClassPolice
  | DeviceClassPowerOutage
  | DeviceClassPowerSupply
  | DeviceClassPregnancy
  | DeviceClassPropane
  | DeviceClassRadiation
  | DeviceClassRadiator
  | DeviceClassRain
  | DeviceClassRefrigerator
  | DeviceClassRemainder
  | DeviceClassRemote
  | DeviceClassRfid
  | DeviceClassSour
  | DeviceClassSalt
  | DeviceClassShampoo
  | DeviceClassShelfLife
  | DeviceClassShopping
  | DeviceClassShort
  | DeviceClassShower
  | DeviceClassSiren
  | DeviceClassSmokeAlarm
  | DeviceClassSoap
  | DeviceClassSoda
  | DeviceClassSolar
  | DeviceClassShutter
  | DeviceClassSoundPressure
  | DeviceClassSpill
  | DeviceClassStale
  | DeviceClassState
  | DeviceClassStatus
  | DeviceClassSulphurDioxide
  | DeviceClassSystem
  | DeviceClassTare
  | DeviceClassTechnical
  | DeviceClassTamper
  | DeviceClassTargetTemperature
  | DeviceClassTask
  | DeviceClassTarget
  | DeviceClassTemperatureAmbient
  | DeviceClassThermalZone
  | DeviceClassTime
  | DeviceClassTimer
  | DeviceClassTobacco
  | DeviceClassToner
  | DeviceClassTorque
  | DeviceClassTransit
  | DeviceClassTreble
  | DeviceClassTuner
  | DeviceClassTv
  | DeviceClassUltraviolet
  | DeviceClassUnitless
  | DeviceClassUnlock
  | DeviceClassUpdate
  | DeviceClassUser
  | DeviceClassUtility
  | DeviceClassVolatileOrganicCompounds
  | DeviceClassVolatileOrganicCompoundsParts
  | DeviceClassVolume
  | DeviceClassVolumeFlowRate
  | DeviceClassWater
  | DeviceClassWeight
  | DeviceClassWifi
  | DeviceClassWine
  | DeviceClassWinter
  | DeviceClassWok
  | DeviceClassWork
  | DeviceClassWasher
  | DeviceClassXyCoordinate
  | DeviceClassYeast
  | DeviceClassOther Text
  deriving (Show, Eq, Generic)

instance ToJSON DeviceClass where
  toJSON DeviceClassBattery = "battery"
  toJSON DeviceClassConnectivity = "connectivity"
  toJSON DeviceClassDoor = "door"
  toJSON DeviceClassGarageDoor = "garage_door"
  toJSON DeviceClassGas = "gas"
  toJSON DeviceClassHeat = "heat"
  toJSON DeviceClassLight = "light"
  toJSON DeviceClassLock = "lock"
  toJSON DeviceClassMoisture = "moisture"
  toJSON DeviceClassMotion = "motion"
  toJSON DeviceClassMoving = "moving"
  toJSON DeviceClassOccupancy = "occupancy"
  toJSON DeviceClassOpening = "opening"
  toJSON DeviceClassPlug = "plug"
  toJSON DeviceClassPower = "power"
  toJSON DeviceClassPresence = "presence"
  toJSON DeviceClassProblem = "problem"
  toJSON DeviceClassSafety = "safety"
  toJSON DeviceClassSmoke = "smoke"
  toJSON DeviceClassSound = "sound"
  toJSON DeviceClassVibration = "vibration"
  toJSON DeviceClassWindow = "window"
  toJSON DeviceClassWindowCovering = "window_covering"
  toJSON DeviceClassTemperature = "temperature"
  toJSON DeviceClassHumidity = "humidity"
  toJSON DeviceClassPressure = "pressure"
  toJSON DeviceClassCurrent = "current"
  toJSON DeviceClassEnergy = "energy"
  toJSON DeviceClassPowerFactor = "power_factor"
  toJSON DeviceClassFrequency = "frequency"
  toJSON DeviceClassVoltage = "voltage"
  toJSON DeviceClassSignalStrength = "signal_strength"
  toJSON DeviceClassDataRate = "data_rate"
  toJSON DeviceClassDataSize = "data_size"
  toJSON DeviceClassDistance = "distance"
  toJSON DeviceClassDuration = "duration"
  toJSON DeviceClassIlluminance = "illuminance"
  toJSON DeviceClassAccelG = "accel_g"
  toJSON DeviceClassAccelMs2 = "accel_ms2"
  toJSON DeviceClassAltitude = "altitude"
  toJSON DeviceClassArea = "area"
  toJSON DeviceClassAqi = "aqi"
  toJSON DeviceClassApparentPower = "apparent_power"
  toJSON DeviceClassAtmosphericPressure = "atmospheric_pressure"
  toJSON DeviceClassBaby = "baby"
  toJSON DeviceClassBaking = "baking"
  toJSON DeviceClassBeauty = "beauty"
  toJSON DeviceClassBeverage = "beverage"
  toJSON DeviceClassBoiler = "boiler"
  toJSON DeviceClassBurglar = "burglar"
  toJSON DeviceClassButton = "button"
  toJSON DeviceClassCabinet = "cabinet"
  toJSON DeviceClassCarbonDioxide = "carbon_dioxide"
  toJSON DeviceClassCarbonMonoxide = "carbon_monoxide"
  toJSON DeviceClassCharger = "charger"
  toJSON DeviceClassCold = "cold"
  toJSON DeviceClassCommunication = "communication"
  toJSON DeviceClassConsumable = "consumable"
  toJSON DeviceClassCooking = "cooking"
  toJSON DeviceClassCounter = "counter"
  toJSON DeviceClassCurrentStage = "current_stage"
  toJSON DeviceClassDishwasher = "dishwasher"
  toJSON DeviceClassDoorbell = "doorbell"
  toJSON DeviceClassDresser = "dresser"
  toJSON DeviceClassDryer = "dryer"
  toJSON DeviceClassEmergency = "emergency"
  toJSON DeviceClassEmergencyCall = "emergency_call"
  toJSON DeviceClassFan = "fan"
  toJSON DeviceClassFault = "fault"
  toJSON DeviceClassFilter = "filter"
  toJSON DeviceClassFire = "fire"
  toJSON DeviceClassFirmware = "firmware"
  toJSON DeviceClassFlag = "flag"
  toJSON DeviceClassGarage = "garage"
  toJSON DeviceClassGasoline = "gasoline"
  toJSON DeviceClassGenerator = "generator"
  toJSON DeviceClassHvac = "hvac"
  toJSON DeviceClassHazel = "hazel"
  toJSON DeviceClassHealth = "health"
  toJSON DeviceClassHeatCold = "heat_cold"
  toJSON DeviceClassHelper = "helper"
  toJSON DeviceClassHolding = "holding"
  toJSON DeviceClassHood = "hood"
  toJSON DeviceClassHub = "hub"
  toJSON DeviceClassInverter = "inverter"
  toJSON DeviceClassIrradiance = "irradiance"
  toJSON DeviceClassKeypad = "keypad"
  toJSON DeviceClassLevel = "level"
  toJSON DeviceClassLifeSafety = "life_safety"
  toJSON DeviceClassLighting = "lighting"
  toJSON DeviceClassLinen = "linen"
  toJSON DeviceClassLockTight = "lock_tight"
  toJSON DeviceClassMains = "mains"
  toJSON DeviceClassMedical = "medical"
  toJSON DeviceClassMode = "mode"
  toJSON DeviceClassMotionSensitivity = "motion_sensitivity"
  toJSON DeviceClassMovingRate = "moving_rate"
  toJSON DeviceClassMower = "mower"
  toJSON DeviceClassMusic = "music"
  toJSON DeviceClassNetwork = "network"
  toJSON DeviceClassNight = "night"
  toJSON DeviceClassNutrition = "nutrition"
  toJSON DeviceClassOil = "oil"
  toJSON DeviceClassOnline = "online"
  toJSON DeviceClassOpeningLevel = "opening_level"
  toJSON DeviceClassOutlet = "outlet"
  toJSON DeviceClassOxygen = "oxygen"
  toJSON DeviceClassPm1 = "pm1"
  toJSON DeviceClassPm10 = "pm10"
  toJSON DeviceClassPm25 = "pm25"
  toJSON DeviceClassParking = "parking"
  toJSON DeviceClassPeak = "peak"
  toJSON DeviceClassPerfume = "perfume"
  toJSON DeviceClassPestControl = "pest_control"
  toJSON DeviceClassPh = "ph"
  toJSON DeviceClassPolice = "police"
  toJSON DeviceClassPowerOutage = "power_outage"
  toJSON DeviceClassPowerSupply = "power_supply"
  toJSON DeviceClassPregnancy = "pregnancy"
  toJSON DeviceClassPropane = "propane"
  toJSON DeviceClassRadiation = "radiation"
  toJSON DeviceClassRadiator = "radiator"
  toJSON DeviceClassRain = "rain"
  toJSON DeviceClassRefrigerator = "refrigerator"
  toJSON DeviceClassRemainder = "remainder"
  toJSON DeviceClassRemote = "remote"
  toJSON DeviceClassRfid = "rfid"
  toJSON DeviceClassSour = "sour"
  toJSON DeviceClassSalt = "salt"
  toJSON DeviceClassShampoo = "shampoo"
  toJSON DeviceClassShelfLife = "shelf_life"
  toJSON DeviceClassShopping = "shopping"
  toJSON DeviceClassShort = "short"
  toJSON DeviceClassShower = "shower"
  toJSON DeviceClassSiren = "siren"
  toJSON DeviceClassSmokeAlarm = "smoke_alarm"
  toJSON DeviceClassSoap = "soap"
  toJSON DeviceClassSoda = "soda"
  toJSON DeviceClassSolar = "solar"
  toJSON DeviceClassShutter = "shutter"
  toJSON DeviceClassSoundPressure = "sound_pressure"
  toJSON DeviceClassSpill = "spill"
  toJSON DeviceClassStale = "stale"
  toJSON DeviceClassState = "state"
  toJSON DeviceClassStatus = "status"
  toJSON DeviceClassSulphurDioxide = "sulphur_dioxide"
  toJSON DeviceClassSystem = "system"
  toJSON DeviceClassTare = "tare"
  toJSON DeviceClassTechnical = "technical"
  toJSON DeviceClassTamper = "tamper"
  toJSON DeviceClassTargetTemperature = "target_temperature"
  toJSON DeviceClassTask = "task"
  toJSON DeviceClassTarget = "target"
  toJSON DeviceClassTemperatureAmbient = "temperature_ambient"
  toJSON DeviceClassThermalZone = "thermal_zone"
  toJSON DeviceClassTime = "time"
  toJSON DeviceClassTimer = "timer"
  toJSON DeviceClassTobacco = "tobacco"
  toJSON DeviceClassToner = "toner"
  toJSON DeviceClassTorque = "torque"
  toJSON DeviceClassTransit = "transit"
  toJSON DeviceClassTreble = "treble"
  toJSON DeviceClassTuner = "tuner"
  toJSON DeviceClassTv = "tv"
  toJSON DeviceClassUltraviolet = "ultraviolet"
  toJSON DeviceClassUnitless = "unitless"
  toJSON DeviceClassUnlock = "unlock"
  toJSON DeviceClassUpdate = "update"
  toJSON DeviceClassUser = "user"
  toJSON DeviceClassUtility = "utility"
  toJSON DeviceClassVolatileOrganicCompounds = "volatile_organic_compounds"
  toJSON DeviceClassVolatileOrganicCompoundsParts = "volatile_organic_compounds_parts"
  toJSON DeviceClassVolume = "volume"
  toJSON DeviceClassVolumeFlowRate = "volume_flow_rate"
  toJSON DeviceClassWater = "water"
  toJSON DeviceClassWeight = "weight"
  toJSON DeviceClassWifi = "wifi"
  toJSON DeviceClassWine = "wine"
  toJSON DeviceClassWinter = "winter"
  toJSON DeviceClassWok = "wok"
  toJSON DeviceClassWork = "work"
  toJSON DeviceClassWasher = "washer"
  toJSON DeviceClassXyCoordinate = "xy_coordinate"
  toJSON DeviceClassYeast = "yeast"
  toJSON (DeviceClassOther text) = toJSON text
