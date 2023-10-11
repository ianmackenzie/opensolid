module Main (main) where

import Data.String (fromString)
import Language.Python.Common hiding ((<>))
import PythonAST qualified as PY
import Prelude
  ( IO
  , Maybe (..)
  , concat
  , fromInteger
  , putStrLn
  , (++)
  )

-- Define the imports and load the ffi lib
setup :: [Statement ()]
setup =
  [ Import [ImportItem [Ident "platform" ()] Nothing ()] ()
  , FromImport (ImportRelative 0 (Just [Ident "ctypes" ()]) ()) (ImportEverything ()) ()
  , Global [Ident "lib" ()] ()
  , PY.set "system" (PY.call "platform.system" [])
  , Conditional
      [
        ( PY.eq (PY.var "system") (PY.string "Darwin")
        , [PY.set "lib" (PY.call "cdll.LoadLibrary" [PY.string "libopensolid-ffi.dylib"])]
        )
      ,
        ( PY.eq (PY.var "system") (PY.string "Linux")
        , [PY.set "lib" (PY.call "cdll.LoadLibrary" [PY.string "libopensolid-ffi.so"])]
        )
      ]
      [ Raise
          ( RaiseV3
              ( Just
                  ( PY.call
                      "Exception"
                      [ PY.string "System "
                          `PY.plus` PY.var "system"
                          `PY.plus` PY.string " is not supported"
                      ]
                  , Nothing
                  )
              )
          )
          ()
      ]
      ()
  , -- the destructor is used in all classes, we only need to declare it once
    PY.set "lib.opensolid_free.argtypes" (List [PY.var "c_void_p"] ())
  ]

point2dClass :: Statement ()
point2dClass =
  -- TODO: make PY.cls a function that takes:
  --     * TH type
  --     * list of TH functions for properties
  --     * list of TH functions for static methods
  PY.cls
    "Point2d"
    ["x_coordinate", "y_coordinate"]
    ( concat
        [ PY.cType "lib.opensolid_point2d_xy" ["c_double", "c_double"] "c_void_p"
        , PY.staticmethod
            "xy"
            [("x", Just "float"), ("y", Just "float")]
            (PY.call "Point2d" [PY.call "lib.opensolid_point2d_xy" [PY.var "x", PY.var "y"]])
            (PY.string "Point2d")
        , PY.cType "lib.opensolid_point2d_x_coordinate" ["c_void_p"] "c_double"
        , PY.property
            "x_coordinate"
            (PY.call "lib.opensolid_point2d_x_coordinate" [PY.var "self.ptr"])
            (PY.var "float")
        , PY.cType "lib.opensolid_point2d_y_coordinate" ["c_void_p"] "c_double"
        , PY.property
            "y_coordinate"
            (PY.call "lib.opensolid_point2d_y_coordinate" [PY.var "self.ptr"])
            (PY.var "float")
        ]
    )

main :: IO ()
main = do
  let pythonCode = prettyText (Module (setup ++ [point2dClass]))
  putStrLn pythonCode
