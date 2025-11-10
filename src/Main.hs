{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ESPBunker.Examples (example1)
import ESPBunker.Language
import Relude

main :: IO ()
main = putBSLn $ generateYAML example1
