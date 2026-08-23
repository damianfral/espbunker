{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ESPBunker
import ESPBunker.Examples
import Relude

main :: IO ()
main = do
  putBSLn $ generateYAML switchExample
  putTextLn $ generateReport switchExample
