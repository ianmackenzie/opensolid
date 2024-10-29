-- Avoid errors when running Fourmolu
{-# LANGUAGE TemplateHaskell #-}

module OpenSolidFFI () where

import OpenSolidAPI (generateForeignFunctions)
import Qty (Qty (Qty)) -- Need to bring internal Qty constructor into scope so that Qty type is valid for FFI

$(generateForeignFunctions)
