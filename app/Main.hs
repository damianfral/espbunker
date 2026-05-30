{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ESPBunker
import ESPBunker.Examples
import Relude

main :: IO ()
main = putBSLn $ generateYAML switchExample
