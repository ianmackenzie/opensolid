module Main (main) where

import File qualified
import List qualified
import OpenSolid
import OpenSolid.API qualified as API
import Pair qualified
import Text qualified

main :: IO ()
main = do
  let builtins =
        [ "opensolid_init"
        , "opensolid_exit"
        , "opensolid_malloc"
        , "opensolid_free"
        , "opensolid_release"
        ]
  let functionNames = builtins + List.map Pair.first API.functions
  let lines = "EXPORTS" : List.map (Text.indent " ") functionNames
  let contents = Text.join "\r\n" lines
  File.writeTo "opensolid-ffi/opensolid-ffi.def" contents
