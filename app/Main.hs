{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ESPBunker
import ESPBunker.Examples
import Relude

main :: IO ()
main = forM_ examples $ \(name, SomeESPM example) -> do
  putStrLn ""
  putTextLn name
  putStrLn ""
  putBSLn $ generateYAML example
  putStrLn ""
  putTextLn $ generateReport example
  putStrLn ""
