{-# LANGUAGE OverloadedStrings #-}
module Main where

import Purebred (getDatabasePath, purebred, defaultConfig)

main :: IO ()
main = do
  fp <- getDatabasePath
  purebred $ defaultConfig fp
