module Main (main) where

import Control.Monad (void)
import Data.String (String, fromString)
import Language.Python.Common hiding (Class, Float, (<>))
import Language.Python.Version3.Parser as Parser
import OpenSolid (List)
import OpenSolidAPI
  ( Api (..)
  , Class (..)
  , Function (..)
  , FunctionKind (..)
  , ValueType (..)
  , openSolidAPI
  )
import PythonAST qualified as PY
import Prelude
  ( Either (..)
  , IO
  , Maybe (..)
  , concatMap
  , error
  , fmap
  , map
  , putStrLn
  , show
  , uncurry
  , unlines
  , (++)
  , (<>)
  )

literalStatement :: String -> List (Statement ())
literalStatement line =
  case Parser.parseStmt line "<literal>" of
    Left err -> error (show err)
    Right (statements, _) -> fmap void statements

literalStatements :: List String -> List (Statement ())
literalStatements = concatMap literalStatement

-- Define the imports and load the ffi lib
setup :: List (Statement ())
setup =
  literalStatements
    [ "from __future__ import annotations"
    , "from typing import Optional"
    , "import platform"
    , "from ctypes import *"
    , "global lib"
    , "system = platform.system()"
    , unlines
        [ "if system == 'Darwin':"
        , "    lib = cdll.LoadLibrary('libopensolid-ffi.dylib')"
        , "elif system == 'Linux':"
        , "    lib = cdll.LoadLibrary('libopensolid-ffi.so')"
        , "else:"
        , "    raise Exception('System ' + system + ' is not supported')"
        ]
    , "lib.opensolid_free.argtypes = [c_void_p]"
    ]

api :: Api -> List (Statement ())
api (Api classes) =
  map apiClass classes

apiClass :: Class -> Statement ()
apiClass (Class name representationProps functions) =
  PY.cls name representationProps (concatMap apiFunction functions)

apiFunction :: Function -> List (Statement ())
apiFunction (Function kind ffiName pyName pyArgs retType) =
  let libName = "lib." <> ffiName
   in PY.cType libName (map (\(_, typ) -> cType typ) pyArgs) (cType retType)
        ++ [ case kind of
              Static ->
                PY.staticmethod
                  ( PY.def
                      pyName
                      (map (\(arg, typ) -> (arg, Just (pyType typ))) pyArgs)
                      (pyType retType)
                      (fnBody retType (PY.call libName (map (uncurry argExpr) pyArgs)))
                  )
              Method ->
                let argsWithoutLast = removeLast pyArgs
                 in PY.def
                      pyName
                      (("self", Nothing) : map (\(arg, typ) -> (arg, Just (pyType typ))) argsWithoutLast)
                      (pyType retType)
                      (fnBody retType (PY.call libName (map (uncurry argExpr) argsWithoutLast ++ [PY.var "self.ptr"])))
           ]

removeLast :: List a -> List a
removeLast [] = []
removeLast [_] = []
removeLast (h : t) = h : removeLast t

fnBody :: ValueType -> Expr () -> [Statement ()]
fnBody typ exp =
  case typ of
    Pointer p -> [Return (Just (PY.call p [exp])) ()]
    Maybe (Pointer p) ->
      [ PY.set "ret_val" exp
      , Return
          ( Just
              ( CondExpr
                  (PY.call p [PY.var "ret_val"]) -- true
                  (PY.var "ret_val") -- condition
                  (PY.var "None") -- false
                  ()
              )
          )
          ()
      ]
    _ -> [Return (Just exp) ()]

argExpr :: String -> ValueType -> Expr ()
argExpr var typ =
  case typ of
    Pointer _ -> Dot (PY.var var) (Ident "ptr" ()) ()
    _ -> PY.var var

cType :: ValueType -> String
cType typ =
  case typ of
    Pointer _ -> "c_void_p"
    Float -> "c_double"
    Boolean -> "c_bool"
    Maybe _ -> "c_void_p"

pyType :: ValueType -> Expr ()
pyType typ =
  case typ of
    Pointer name -> PY.var name
    Float -> PY.var "float"
    Boolean -> PY.var "bool"
    Maybe nestedTyp -> Subscript (PY.var "Optional") (pyType nestedTyp) ()

main :: IO ()
main = do
  let pythonCode = prettyText (Module (setup ++ api openSolidAPI))
  putStrLn pythonCode
