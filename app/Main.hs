{-# LANGUAGE OverloadedStrings #-}
module Main where

import Purebred (purebred, defaultConfig)

main :: IO ()
main = purebred defaultConfig
