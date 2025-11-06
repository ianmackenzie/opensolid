-- Avoid errors when running Fourmolu
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}

module ForeignExports () where

import FFI qualified
import Foreign (StablePtr)
import Foreign qualified

foreign export ccall opensolid_release :: StablePtr a -> IO ()

opensolid_release :: StablePtr a -> IO ()
opensolid_release = Foreign.freeStablePtr

$(FFI.generateExports)
