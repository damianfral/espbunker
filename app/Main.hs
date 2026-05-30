{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ESPBunker.Examples
import ESPBunker.Language
import Relude

main :: IO ()
main = putBSLn $ generateYAML switchExample
