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
  , Num (fromInteger)
  , concatMap
  , error
  , fmap
  , fromInteger
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
    , "from contextlib import contextmanager"
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
    , "global_tolerance = None"
    , unlines
        [ "@contextmanager"
        , "def Tolerance(new_tolerance: float):"
        , "    global global_tolerance"
        , "    (saved_tolerance, global_tolerance) = (global_tolerance, new_tolerance)"
        , "    try:"
        , "        yield"
        , "    finally:"
        , "        global_tolerance = saved_tolerance"
        ]
    , unlines
        [ "class RESULT(Structure):"
        , "    _fields_ = [('ptr', c_void_p), ('tag', c_int8)]"
        ]
    , unlines
        [ "class IsZero(Exception):"
        , "    pass"
        ]
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
        ++ [ let
              (tolerance, restArgs) = splitTolerance pyArgs
              -- add tolerance as the last argument of the python function
              withTolArg (Just (arg, typ)) list = list ++ [(arg, Just (pyType typ), Just (PY.var "None"))]
              withTolArg Nothing list = list

              -- add tolerance as the first argument of the c function call
              withTolExp (Just t) list = t : list
              withTolExp Nothing list = list

              -- check if the tolerance argument is passed or the global_tolerance is set
              withTolBody (Just (arg, _)) stmts =
                Conditional
                  [
                    ( PY.is (PY.var arg) (PY.var "None") `PY.and` PY.is (PY.var "global_tolerance") (PY.var "None")
                    , literalStatement "raise Exception('Tolerance is not set')"
                    )
                  ]
                  []
                  ()
                  : stmts
              withTolBody Nothing stmts = stmts
              in
              case kind of
                Static ->
                  PY.staticmethod
                    ( PY.def
                        pyName
                        (withTolArg tolerance (map (\(arg, typ) -> (arg, Just (pyType typ), Nothing)) restArgs))
                        (pyType retType)
                        (withTolBody tolerance (fnBody retType (PY.call libName (map (uncurry argExpr) (withTolExp tolerance restArgs)))))
                    )
                Method ->
                  let argsWithoutLast = removeLast restArgs
                   in PY.def
                        pyName
                        (withTolArg tolerance (("self", Nothing, Nothing) : map (\(arg, typ) -> (arg, Just (pyType typ), Nothing)) argsWithoutLast))
                        (pyType retType)
                        (withTolBody tolerance (fnBody retType (PY.call libName (map (uncurry argExpr) (withTolExp tolerance argsWithoutLast) ++ [PY.var "self.ptr"]))))
           ]

removeLast :: List a -> List a
removeLast [] = []
removeLast [_] = []
removeLast (h : t) = h : removeLast t

-- split out the first argument if its tolerance
splitTolerance :: List (String, ValueType) -> (Maybe (String, ValueType), List (String, ValueType))
splitTolerance [] = (Nothing, [])
splitTolerance ((tol, ImplicitTolerance) : rest) = (Just (tol, ImplicitTolerance), rest)
splitTolerance rest = (Nothing, rest)

fnBody :: ValueType -> Expr () -> [Statement ()]
fnBody typ exp =
  case typ of
    Pointer p -> [Return (Just (PY.call p [exp])) ()]
    Result err (Pointer p) ->
      [ PY.set "ret_val" exp
      , PY.set "result" (PY.var "ret_val.contents")
      , -- TODO: free the memory
        Conditional
          [
            ( PY.var "result.tag" `PY.eq` PY.int 0
            , [Return (Just (PY.call p [PY.var "result.ptr"])) ()]
            )
          ]
          -- TODO: construct an error based on tag and throw it
          (literalStatement ("raise " <> err <> "()"))
          ()
      ]
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
    ImplicitTolerance -> PY.var var `PY.or` PY.var "global_tolerance"
    _ -> PY.var var

cType :: ValueType -> Expr ()
cType typ =
  case typ of
    ImplicitTolerance -> PY.var "c_double"
    Pointer _ -> PY.var "c_void_p"
    Float -> PY.var "c_double"
    Boolean -> PY.var "c_bool"
    Maybe _ -> PY.var "c_void_p"
    Result _ _ -> PY.call "POINTER" [PY.var "RESULT"]

pyType :: ValueType -> Expr ()
pyType typ =
  case typ of
    ImplicitTolerance -> Subscript (PY.var "Optional") (PY.var "float") ()
    Pointer name -> PY.var name
    Float -> PY.var "float"
    Boolean -> PY.var "bool"
    Maybe nestedTyp -> Subscript (PY.var "Optional") (pyType nestedTyp) ()
    Result _ val -> pyType val -- we throw an exception in case of an err

main :: IO ()
main = do
  let pythonCode = prettyText (Module (setup ++ api openSolidAPI))
  putStrLn pythonCode
