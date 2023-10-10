module Main (main) where

import Data.String (String, fromString)
import Language.Python.Common hiding ((<>))
import Prelude (IO, Maybe (..), fromInteger, putStrLn, (++), (<>))

string :: String -> Expr ()
string st =
  Strings ["\"" <> st <> "\""] ()

-- Define the imports
imports :: [Statement ()]
imports =
  [ Import [ImportItem [Ident "platform" ()] Nothing ()] ()
  , FromImport (ImportRelative 0 (Just [Ident "ctypes" ()]) ()) (ImportEverything ()) ()
  ]

-- Define the global variable 'lib'
setLib :: String -> Statement ()
setLib filename =
  Assign
    [Var (Ident "lib" ()) ()]
    ( Call
        (Var (Ident "cdll.LoadLibrary" ()) ())
        [ArgExpr (string filename) ()]
        ()
    )
    ()

-- Define the 'system' variable and 'if' statement
loadLib :: [Statement ()]
loadLib =
  [ Global [Ident "lib" ()] ()
  , Assign
      [Var (Ident "system" ()) ()]
      (Call (Var (Ident "platform.system" ()) ()) [] ())
      ()
  , Conditional
      [
        ( BinaryOp (Equality ()) (Var (Ident "system" ()) ()) (string "Darwin") ()
        , [setLib "libopensolid-ffi.dylib"]
        )
      ,
        ( BinaryOp (Equality ()) (Var (Ident "system" ()) ()) (string "Linux") ()
        , [setLib "libopensolid-ffi.so"]
        )
      ]
      [ Raise
          ( RaiseV3
              ( Just
                  ( Call
                      (Var (Ident "Exception" ()) ())
                      [ ArgExpr
                          ( BinaryOp
                              (Plus ())
                              (string "System ")
                              (BinaryOp (Plus ()) (Var (Ident "system" ()) ()) (string " is not supported") ())
                              ()
                          )
                          ()
                      ]
                      ()
                  , Nothing
                  )
              )
          )
          ()
      ]
      ()
  ]

-- Define 'Point2d' class
point2dClass :: Statement ()
point2dClass =
  Class
    (Ident "Point2d" ())
    []
    [ -- constructor from pointer
      Fun
        (Ident "__init__" ())
        [Param (Ident "self" ()) Nothing Nothing (), Param (Ident "ptr" ()) (Just (Var (Ident "c_void_p" ()) ())) Nothing ()]
        Nothing
        [ Assign
            [Var (Ident "self.ptr" ()) ()]
            (Var (Ident "ptr" ()) ())
            ()
        ]
        ()
    , -- Point2d.xy
      Assign
        [Var (Ident "lib.opensolid_point2d_xy.argtypes" ()) ()]
        (List [Var (Ident "c_double" ()) (), Var (Ident "c_double" ()) ()] ())
        ()
    , Assign
        [Var (Ident "lib.opensolid_point2d_xy.restype" ()) ()]
        (Var (Ident "c_void_p" ()) ())
        ()
    , Decorated
        [Decorator [Ident "staticmethod" ()] [] ()]
        ( Fun
            (Ident "xy" ())
            [Param (Ident "x" ()) (Just (Var (Ident "float" ()) ())) Nothing (), Param (Ident "y" ()) (Just (Var (Ident "float" ()) ())) Nothing ()]
            (Just (string "Point2d"))
            [ Return
                ( Just
                    ( Call
                        (Var (Ident "Point2d" ()) ())
                        [ ArgExpr
                            ( Call
                                (Var (Ident "lib.opensolid_point2d_xy" ()) ())
                                [ ArgExpr (Var (Ident "x" ()) ()) ()
                                , ArgExpr (Var (Ident "y" ()) ()) ()
                                ]
                                ()
                            )
                            ()
                        ]
                        ()
                    )
                )
                ()
            ]
            ()
        )
        ()
    , -- Point2d.xCoordinate
      Assign
        [Var (Ident "lib.opensolid_point2d_x_coordinate.argtypes" ()) ()]
        (List [Var (Ident "c_void_p" ()) ()] ())
        ()
    , Assign
        [Var (Ident "lib.opensolid_point2d_x_coordinate.restype" ()) ()]
        (Var (Ident "c_double" ()) ())
        ()
    , Decorated
        [Decorator [Ident "property" ()] [] ()]
        ( Fun
            (Ident "x_coordinate" ())
            [Param (Ident "self" ()) Nothing Nothing ()]
            (Just (Var (Ident "float" ()) ()))
            [ Return
                ( Just
                    ( Call
                        (Var (Ident "lib.opensolid_point2d_x_coordinate" ()) ())
                        [ArgExpr (Var (Ident "self.ptr" ()) ()) ()]
                        ()
                    )
                )
                ()
            ]
            ()
        )
        ()
    , -- Point2d.yCoordinate
      Assign
        [Var (Ident "lib.opensolid_point2d_y_coordinate.argtypes" ()) ()]
        (List [Var (Ident "c_void_p" ()) ()] ())
        ()
    , Assign
        [Var (Ident "lib.opensolid_point2d_y_coordinate.restype" ()) ()]
        (Var (Ident "c_double" ()) ())
        ()
    , Decorated
        [Decorator [Ident "property" ()] [] ()]
        ( Fun
            (Ident "y_coordinate" ())
            [Param (Ident "self" ()) Nothing Nothing ()]
            (Just (Var (Ident "float" ()) ()))
            [ Return
                ( Just
                    ( Call
                        (Var (Ident "lib.opensolid_point2d_y_coordinate" ()) ())
                        [ArgExpr (Var (Ident "self.ptr" ()) ()) ()]
                        ()
                    )
                )
                ()
            ]
            ()
        )
        ()
    , -- OpenSolid.free
      Assign
        [Var (Ident "lib.opensolid_free.argtypes" ()) ()]
        (List [Var (Ident "c_void_p" ()) ()] ())
        ()
    , Fun
        (Ident "__del__" ())
        [Param (Ident "self" ()) Nothing Nothing ()]
        Nothing
        [ StmtExpr
            ( Call
                (Var (Ident "lib.opensolid_free" ()) ())
                [ArgExpr (Var (Ident "self.ptr" ()) ()) ()]
                ()
            )
            ()
        ]
        ()
    , -- representation

      Fun
        (Ident "__repr__" ())
        [Param (Ident "self" ()) Nothing Nothing ()]
        Nothing
        [ Return
            ( Just
                ( BinaryOp
                    (Plus ())
                    (string "Point2d(")
                    ( BinaryOp
                        (Plus ())
                        (Call (Var (Ident "str" ()) ()) [ArgExpr (Var (Ident "self.x_coordinate" ()) ()) ()] ())
                        ( BinaryOp
                            (Plus ())
                            (string ", ")
                            ( BinaryOp
                                (Plus ())
                                (Call (Var (Ident "str" ()) ()) [ArgExpr (Var (Ident "self.y_coordinate" ()) ()) ()] ())
                                (string ")")
                                ()
                            )
                            ()
                        )
                        ()
                    )
                    ()
                )
            )
            ()
        ]
        ()
    ]
    ()

main :: IO ()
main = do
  let pythonCode = prettyText (Module (imports ++ loadLib ++ [point2dClass]))
  putStrLn pythonCode
