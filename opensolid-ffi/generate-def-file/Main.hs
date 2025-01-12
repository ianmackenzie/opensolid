module Main (main) where

import API qualified
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

main :: IO ()
main = do
  let builtins =
        [ "opensolid_init"
        , "opensolid_exit"
        , "opensolid_malloc"
        , "opensolid_free"
        , "opensolid_release"
        ]
  let functionNames = builtins + List.map API.ffiName API.functions
  let lines = "EXPORTS" : List.map (Text.indent " ") functionNames
  let contents = Text.join "\r\n" lines
  IO.writeFile "opensolid-ffi/opensolid-ffi.def" contents
