module PythonAST
  ( string
  , var
  , call
  , plus
  , eq
  , is
  , set
  , cls
  , and
  , or
  , constructor
  , cType
  , destructor
  , def
  , staticmethod
  )
where

import Data.List (intersperse)
import Data.String (String, fromString)
import Language.Python.Common hiding ((<>))
import OpenSolid (List)
import Prelude
  ( Foldable (foldl)
  , Functor (..)
  , Maybe (..)
  , concat
  , (<>)
  )

-- ctypes type definition for a function
-- `cType "lib.opensolid_point2d_xy" ["c_double", "c_double"] "c_void_p"` becomes:
--     lib.opensolid_point2d_xy.argtypes = [c_double, c_double]
--     lib.opensolid_point2d_xy.restype = c_void_p
cType :: String -> List String -> String -> List (Statement ())
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
call :: String -> List (Expr ()) -> Expr ()
call name args =
  Call
    (var name)
    (fmap (\e -> ArgExpr e ()) args)
    ()

-- function parameter with an optional type declaration
param :: String -> Maybe (Expr ()) -> Maybe (Expr ()) -> Parameter ()
param name typ defalutValue =
  Param (Ident name ()) typ defalutValue ()

-- Python function
def :: String -> List (String, Maybe (Expr ()), Maybe (Expr ())) -> Expr () -> List (Statement ()) -> Statement ()
def name params retType body =
  Fun
    (Ident name ())
    (fmap (\(n, t, v) -> param n t v) params)
    (Just retType)
    body
    ()

-- Assignment statement `var = expr`
set :: String -> Expr () -> Statement ()
set name expr =
  Assign [var name] expr ()

-- Python class declaration
cls :: String -> List String -> Suite () -> Statement ()
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

-- A static method decorator
staticmethod :: Statement () -> Statement ()
staticmethod func =
  Decorated [Decorator [Ident "staticmethod" ()] [] ()] func ()

-- A plus operator `a + b`
plus :: Expr () -> Expr () -> Expr ()
plus a b =
  BinaryOp (Plus ()) a b ()

-- An equality operator `a == b`
eq :: Expr () -> Expr () -> Expr ()
eq a b =
  BinaryOp (Equality ()) a b ()

-- An is operator `a is b`
is :: Expr () -> Expr () -> Expr ()
is a b =
  BinaryOp (Is ()) a b ()

-- An and operator `a and b`
and :: Expr () -> Expr () -> Expr ()
and a b =
  BinaryOp (And ()) a b ()

-- An or operator `a or b`
or :: Expr () -> Expr () -> Expr ()
or a b =
  BinaryOp (Or ()) a b ()

-- A constructor from pointer
constructor :: Statement ()
constructor =
  def
    "__init__"
    [("self", Nothing, Nothing), ("ptr", Just (var "c_void_p"), Nothing)]
    (var "None")
    [set "self.ptr" (var "ptr")]

-- Destructor
destructor :: Statement ()
destructor =
  def
    "__del__"
    [("self", Nothing, Nothing)]
    (var "None")
    [StmtExpr (call "lib.opensolid_free" [var "self.ptr"]) ()]

-- Representation method, defined by class name and properties, that are stringified
represenation :: String -> List String -> Statement ()
represenation name properties =
  def
    "__repr__"
    [("self", Nothing, Nothing)]
    (var "str")
    [ Return
        ( Just
            ( foldl
                plus
                (string (name <> "("))
                ( intersperse
                    (string ", ")
                    (fmap (\prop -> call "str" [call ("self." <> prop) []]) properties)
                )
                `plus` string ")"
            )
        )
        ()
    ]
