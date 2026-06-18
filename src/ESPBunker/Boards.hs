{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Boards where

import ESPBunker.Components (Board)

type ESP32C3 =
  Board
    "esp32-c3-devkitm-1"
    '[]
    '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20, 21]

type ESP32Dev =
  Board
    "esp32dev"
    '[]
    '[0, 2, 4, 5, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27, 32, 33]

type ESP32S3 =
  Board
    "esp32-s3-devkitc-1"
    '[]
    '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 21, 35, 36, 37, 38, 39, 40, 41, 42, 45, 46, 47, 48]

type ESP32S2 =
  Board
    "esp32-s2-saola-1"
    '[]
    '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 26, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45]

type NodeMCU =
  Board
    "nodemcuv2"
    '[]
    '[0, 1, 2, 3, 4, 5, 12, 13, 14, 15, 16]

type D1Mini =
  Board
    "d1_mini"
    '[]
    '[0, 1, 2, 3, 4, 5, 12, 13, 14, 15, 16]

type ESP01 =
  Board
    "esp01_1m"
    '[]
    '[0, 2]

type FeatherESP32 =
  Board
    "featheresp32"
    '[]
    '[0, 2, 4, 5, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27, 32, 33]

type Lolin32 =
  Board
    "lolin32"
    '[]
    '[0, 1, 2, 3, 4, 5, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27, 32, 33]

type TinyPICO =
  Board
    "tinypico"
    '[]
    '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 19, 20, 21, 26, 33, 34, 35, 36, 37, 38, 39]

type M5StackCore =
  Board
    "m5stack-core-esp32"
    '[]
    '[0, 2, 4, 5, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27, 32, 33, 34, 35, 36, 37, 38, 39]

type WemosD1Mini32 =
  Board
    "wemos_d1_mini32"
    '[]
    '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27, 32, 33]
