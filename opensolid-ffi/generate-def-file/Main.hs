module Main (main) where

import OpenSolid.API qualified as API
import OpenSolid.API.Function qualified as Function
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
  let functionNames = builtins <> List.map Function.ffiName API.functions
  let lines = "EXPORTS" : List.map (Text.indent " ") functionNames
  let contents = Text.join "\r\n" lines
  IO.writeUtf8 "opensolid-ffi/opensolid-ffi.def" contents
