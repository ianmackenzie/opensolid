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
    , "from contextlib import contextmanager"
    , "from typing import Optional, Callable, TypeVar, Tuple"
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
    , "lib.opensolid_free_stable.argtypes = [c_void_p]"
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
    , "A = TypeVar('A')"
    , "B = TypeVar('B')"
    , unlines
        [ "def maybe_reader(read_success: Callable[[c_void_p], A]) -> Callable[[c_void_p], Optional[A]]:"
        , "    return lambda ptr: read_success(ptr) if ptr else None"
        ]
    , unlines
        [ "class Tuple2Structure(Structure):"
        , "    _fields_ = [('ptr1', c_void_p), ('ptr2', c_void_p)]"
        ]
    , unlines
        [ "def tuple2_reader(read_a: Callable[[c_void_p], A], read_b: Callable[[c_void_p], B]) -> Callable[[c_void_p], Tuple[A, B]]:"
        , "    def read(ptr: c_void_p) -> Tuple[A, B]:"
        , "        tuple2_struct = cast(ptr, POINTER(Tuple2Structure)).contents"
        , "        res = (read_a(tuple2_struct.ptr1), read_b(tuple2_struct.ptr2))"
        , "        lib.opensolid_free(ptr)"
        , "        return res"
        , "    return read"
        ]
    , unlines
        [ "class ResultStructure(Structure):"
        , "    _fields_ = [('ptr', c_void_p), ('tag', c_int8)]"
        ]
    , unlines
        [ "def result_reader(read_error: Callable[[c_int8, c_void_p], Exception], read_success: Callable[[c_void_p], A]) -> Callable[[c_void_p], A]:"
        , "    def read(ptr: c_void_p) -> A:"
        , "        result_struct = cast(ptr, POINTER(ResultStructure)).contents"
        , "        tag = result_struct.tag"
        , "        ptr1 = result_struct.ptr"
        , "        lib.opensolid_free(ptr)"
        , "        if tag == 0:"
        , "            return read_success(ptr1)"
        , "        else:"
        , "            raise read_error(tag, ptr1)"
        , "    return read"
        ]
    , unlines
        [ "def read_float(ptr: c_void_p) -> float:"
        , "    val = float(cast(ptr, POINTER(c_double)).contents.value)"
        , "    lib.opensolid_free(ptr)"
        , "    return val"
        ]
    , unlines
        [ "def read_bool(ptr: c_void_p) -> bool:"
        , "    val = bool(cast(ptr, POINTER(c_bool)).contents.value)"
        , "    lib.opensolid_free(ptr)"
        , "    return val"
        ]
    , -- TODO: codegen this
      unlines
        [ "class IsZero(Exception):"
        , "    def __init__(self, tag: c_int8, ptr: c_void_p):"
        , "        lib.opensolid_free(ptr)"
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
                        (withTolBody tolerance (fnBody retType (PY.call (PY.var libName) (map (uncurry argExpr) (withTolExp tolerance restArgs)))))
                    )
                Method ->
                  let argsWithoutLast = removeLast restArgs
                   in PY.def
                        pyName
                        (withTolArg tolerance (("self", Nothing, Nothing) : map (\(arg, typ) -> (arg, Just (pyType typ), Nothing)) argsWithoutLast))
                        (pyType retType)
                        (withTolBody tolerance (fnBody retType (PY.call (PY.var libName) (map (uncurry argExpr) (withTolExp tolerance argsWithoutLast) ++ [PY.var "self.ptr"]))))
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

exprReader :: ValueType -> Expr ()
exprReader typ =
  case typ of
    Pointer p -> PY.var p
    Tuple2 a b -> PY.call (PY.var "tuple2_reader") [exprReader a, exprReader b]
    Result err succ -> PY.call (PY.var "result_reader") [PY.var err, exprReader succ]
    Maybe succ -> PY.call (PY.var "maybe_reader") [exprReader succ]
    Float -> PY.var "read_float"
    Boolean -> PY.var "read_bool"
    ImplicitTolerance -> PY.var "read_float"

fnBody :: ValueType -> Expr () -> [Statement ()]
fnBody typ exp =
  [Return (Just expression) ()]
 where
  expression = case typ of
    Pointer _ -> PY.call (exprReader typ) [exp]
    Tuple2 _ _ -> PY.call (exprReader typ) [exp]
    Result _ _ -> PY.call (exprReader typ) [exp]
    Maybe _ -> PY.call (exprReader typ) [exp]
    _ -> exp

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
    Result _ _ -> PY.var "c_void_p"
    Tuple2 _ _ -> PY.var "c_void_p"

pyType :: ValueType -> Expr ()
pyType typ =
  case typ of
    ImplicitTolerance -> Subscript (PY.var "Optional") (PY.var "float") ()
    Pointer name -> PY.var name
    Float -> PY.var "float"
    Boolean -> PY.var "bool"
    Maybe nestedTyp -> Subscript (PY.var "Optional") (pyType nestedTyp) ()
    Result _ val -> pyType val -- we throw an exception in case of an err
    Tuple2 val1 val2 -> Subscript (PY.var "Tuple") (Tuple [pyType val1, pyType val2] ()) ()

main :: IO ()
main = do
  let pythonCode = prettyText (Module (setup ++ api openSolidAPI))
  putStrLn pythonCode
