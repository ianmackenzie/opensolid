module PythonAST
  ( string
  , var
  , call
  , plus
  , eq
  , set
  , cls
  , constructor
  , cType
  , destructor
  , method
  , staticmethod
  )
where

import Data.List (intersperse)
import Data.String (String, fromString)
import Language.Python.Common hiding ((<>))
import Prelude
  ( Foldable (foldl)
  , Functor (..)
  , Maybe (..)
  , concat
  , uncurry
  , (<>)
  )

-- ctypes type definition for a function
-- `cType "lib.opensolid_point2d_xy" ["c_double", "c_double"] "c_void_p"` becomes:
--     lib.opensolid_point2d_xy.argtypes = [c_double, c_double]
--     lib.opensolid_point2d_xy.restype = c_void_p
cType :: String -> [String] -> String -> [Statement ()]
cType fname argTypes resType =
  [ set (fname <> ".argtypes") (List (fmap var argTypes) ())
  , set (fname <> ".restype") (var resType)
  ]

-- a string literal, surrounded in double quotes, because
-- language-python doesn't do that automatically
-- TODO: should we protect against double quotes within a string?
string :: String -> Expr ()
string st =
  Strings ["\"" <> st <> "\""] ()

-- variable
var :: String -> Expr ()
var name =
  Var (Ident name ()) ()

-- function call, takes a function name and a list of arguments
call :: String -> [Expr ()] -> Expr ()
call name args =
  Call
    (var name)
    (fmap (\e -> ArgExpr e ()) args)
    ()

-- function parameter with an optional type declaration
param :: String -> Maybe String -> Parameter ()
param name typ =
  Param (Ident name ()) (fmap var typ) Nothing ()

-- Python function with statements that doesn't return anything
def :: String -> [(String, Maybe String)] -> [Statement ()] -> Statement ()
def name params body =
  Fun (Ident name ()) (fmap (uncurry param) params) Nothing body ()

-- Python function that returns an expression
fn :: String -> [(String, Maybe String)] -> Expr () -> Expr () -> Statement ()
fn name params expression retType =
  Fun
    (Ident name ())
    (fmap (uncurry param) params)
    (Just retType)
    [Return (Just expression) ()]
    ()

-- Assignment statement `var = expr`
set :: String -> Expr () -> Statement ()
set name expr =
  Assign [var name] expr ()

-- Python class declaration
cls :: String -> [String] -> Suite () -> Statement ()
cls name representationProps members =
  Class
    (Ident name ())
    []
    ( concat
        [ [constructor]
        , members
        ,
          [ destructor
          , represenation name representationProps
          ]
        ]
    )
    ()

-- A method
method :: String -> [(String, Maybe String)] -> Expr () -> String -> [Statement ()]
method name params ret retType =
  [ fn name params ret (var retType)
  ]

-- A method that is attached to a class
staticmethod :: String -> [(String, Maybe String)] -> Expr () -> String -> [Statement ()]
staticmethod name params ret retType =
  [ Decorated
      [Decorator [Ident "staticmethod" ()] [] ()]
      (fn name params ret (var retType))
      ()
  ]

-- A plus operator `a + b`
plus :: Expr () -> Expr () -> Expr ()
plus a b =
  BinaryOp (Plus ()) a b ()

-- An equality operator `a == b`
eq :: Expr () -> Expr () -> Expr ()
eq a b =
  BinaryOp (Equality ()) a b ()

-- A constructor from pointer
constructor :: Statement ()
constructor =
  def
    "__init__"
    [("self", Nothing), ("ptr", Just "c_void_p")]
    [set "self.ptr" (var "ptr")]

-- Destructor
destructor :: Statement ()
destructor =
  def
    "__del__"
    [("self", Nothing)]
    [StmtExpr (call "lib.opensolid_free" [var "self.ptr"]) ()]

-- Representation method, defined by class name and properties, that are stringified
represenation :: String -> [String] -> Statement ()
represenation name properties =
  fn
    "__repr__"
    [("self", Nothing)]
    ( foldl
        plus
        (string (name <> "("))
        ( intersperse
            (string ", ")
            (fmap (\prop -> call "str" [call ("self." <> prop) []]) properties)
        )
        `plus` string ")"
    )
    (var "str")
