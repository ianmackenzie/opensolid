module Main (main) where

import Debug qualified
import List qualified
import OpenSolid
import OpenSolidAPI
  ( Api (..)
  , Class (..)
  , ExceptionClass (..)
  , Function (..)
  , FunctionKind (..)
  , ValueType (..)
  , openSolidAPI
  )
import PythonAST qualified as PY
import Text qualified
import Prelude qualified

-- Define the imports and load the ffi lib
setup :: List PY.Statement
setup =
  PY.literalStatements
    [ "from __future__ import annotations"
    , "from contextlib import contextmanager"
    , "from typing import Optional, Callable, TypeVar, Tuple"
    , "import platform"
    , "from ctypes import c_bool, c_double, c_int8, c_void_p, cast, cdll, POINTER, Structure"
    , "global lib"
    , "system = platform.system()"
    , Text.join "\n" $
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
    , Text.join "\n" $
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
    , Text.join "\n" $
        [ "def maybe_reader(read_success: Callable[[c_void_p], A]) -> Callable[[c_void_p], Optional[A]]:"
        , "    return lambda ptr: read_success(ptr) if ptr else None"
        ]
    , Text.join "\n" $
        [ "class Tuple2Structure(Structure):"
        , "    _fields_ = [('ptr1', c_void_p), ('ptr2', c_void_p)]"
        ]
    , Text.join "\n" $
        [ "def tuple2_reader(read_a: Callable[[c_void_p], A], read_b: Callable[[c_void_p], B]) -> Callable[[c_void_p], Tuple[A, B]]:"
        , "    def read(ptr: c_void_p) -> Tuple[A, B]:"
        , "        tuple2_struct = cast(ptr, POINTER(Tuple2Structure)).contents"
        , "        res = (read_a(tuple2_struct.ptr1), read_b(tuple2_struct.ptr2))"
        , "        lib.opensolid_free(ptr)"
        , "        return res"
        , "    return read"
        ]
    , Text.join "\n" $
        [ "class ResultStructure(Structure):"
        , "    _fields_ = [('ptr', c_void_p), ('tag', c_int8)]"
        ]
    , Text.join "\n" $
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
    , Text.join "\n" $
        [ "def read_float(ptr: c_void_p) -> float:"
        , "    val = float(cast(ptr, POINTER(c_double)).contents.value)"
        , "    lib.opensolid_free(ptr)"
        , "    return val"
        ]
    , Text.join "\n" $
        [ "def read_bool(ptr: c_void_p) -> bool:"
        , "    val = bool(cast(ptr, POINTER(c_bool)).contents.value)"
        , "    lib.opensolid_free(ptr)"
        , "    return val"
        ]
    ]

api :: Api -> List PY.Statement
api (Api classes) =
  List.map apiClass classes

apiClass :: Class -> PY.Statement
apiClass (Class name representationProps errorClasses functions) =
  PY.cls name [] $
    constructor
      : destructor
      : represenation name representationProps
      : List.collect apiFunction functions
      ++ List.collect (apiException name) errorClasses

apiException :: Text -> ExceptionClass -> List PY.Statement
apiException mod (ExceptionClass name [(tag, constructorName, Nothing)])
  | name == constructorName =
      let ret_typ = PY.var mod `PY.dot` name
       in [ PY.cls name [PY.var "Exception"] $
              [ PY.staticmethod
                  ( PY.def
                      "from_tag"
                      [ ("tag", Just (PY.var "c_int8"), Nothing)
                      , ("ptr", Just (PY.var "c_void_p"), Nothing)
                      ]
                      ret_typ
                      [ PY.conditional
                          [
                            ( PY.var "tag" `PY.eq` PY.int tag
                            , [PY.return (PY.call ret_typ [])]
                            )
                          ]
                          (PY.literalStatement "raise ValueError('Unkown exception tag ' + str(tag))")
                      ]
                  )
              ]
          ]
-- TODO: support exceptions with mutiple cases and pointers
apiException _ ex = Prelude.error (Text.toChars (Debug.show ex))

apiFunction :: Function -> List PY.Statement
apiFunction (Function kind ffiName pyName pyArgs retType) =
  let libName = PY.var "lib" `PY.dot` ffiName
   in PY.cFunctionType libName (List.map (\(_, typ) -> cType typ) pyArgs) (cType retType)
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
                PY.conditional
                  [
                    ( PY.is (PY.var arg) (PY.var "None") `PY.and` PY.is (PY.var "global_tolerance") (PY.var "None")
                    , PY.literalStatement "raise Exception('Tolerance is not set')"
                    )
                  ]
                  []
                  : stmts
              withTolBody Nothing stmts = stmts
              in
              case kind of
                Static ->
                  PY.staticmethod
                    ( PY.def
                        pyName
                        (withTolArg tolerance (List.map (\(arg, typ) -> (arg, Just (pyType typ), Nothing)) restArgs))
                        (pyType retType)
                        (withTolBody tolerance (fnBody retType (PY.call libName (List.map (\(a, t) -> argExpr a t) (withTolExp tolerance restArgs)))))
                    )
                Method ->
                  let argsWithoutLast = removeLast restArgs
                   in PY.def
                        pyName
                        (withTolArg tolerance (("self", Nothing, Nothing) : List.map (\(arg, typ) -> (arg, Just (pyType typ), Nothing)) argsWithoutLast))
                        (pyType retType)
                        (withTolBody tolerance (fnBody retType (PY.call libName (List.map (\(a, t) -> argExpr a t) (withTolExp tolerance argsWithoutLast) ++ [PY.var "self.ptr"]))))
           ]

-- A constructor from pointer
constructor :: PY.Statement
constructor =
  PY.def
    "__init__"
    [("self", Nothing, Nothing), ("ptr", Just (PY.var "c_void_p"), Nothing)]
    (PY.var "None")
    [PY.set (PY.var "self" `PY.dot` "ptr") (PY.var "ptr")]

-- Destructor
destructor :: PY.Statement
destructor =
  PY.def
    "__del__"
    [("self", Nothing, Nothing)]
    (PY.var "None")
    [PY.stmtExpr (PY.call (PY.var "lib" `PY.dot` "opensolid_free_stable") [PY.var "self" `PY.dot` "ptr"])]

-- Representation method, defined by class name and properties, that are stringified
represenation :: Text -> List Text -> PY.Statement
represenation name properties =
  PY.def
    "__repr__"
    [("self", Nothing, Nothing)]
    (PY.var "str")
    [ PY.return
        ( List.foldLeft
            PY.plus
            (PY.string (Text.concat [name, "("]))
            ( List.intersperse
                (PY.string ", ")
                (List.map (\prop -> PY.call (PY.var "str") [PY.call (PY.var "self" `PY.dot` prop) []]) properties)
            )
            `PY.plus` PY.string ")"
        )
    ]

removeLast :: List a -> List a
removeLast [] = []
removeLast [_] = []
removeLast (h : t) = h : removeLast t

-- split out the first argument if its tolerance
splitTolerance :: List (Text, ValueType) -> (Maybe (Text, ValueType), List (Text, ValueType))
splitTolerance [] = (Nothing, [])
splitTolerance ((tol, ImplicitTolerance) : rest) = (Just (tol, ImplicitTolerance), rest)
splitTolerance rest = (Nothing, rest)

exprReader :: ValueType -> PY.Expr
exprReader typ =
  case typ of
    Pointer p -> PY.var p
    Tuple2 a b -> PY.call (PY.var "tuple2_reader") [exprReader a, exprReader b]
    Result mod err succ ->
      PY.call
        (PY.var "result_reader")
        [ PY.var mod `PY.dot` err `PY.dot` "from_tag"
        , exprReader succ
        ]
    Maybe succ -> PY.call (PY.var "maybe_reader") [exprReader succ]
    Float -> PY.var "read_float"
    Boolean -> PY.var "read_bool"
    ImplicitTolerance -> PY.var "read_float"

fnBody :: ValueType -> PY.Expr -> [PY.Statement]
fnBody typ exp =
  [PY.return expression]
 where
  expression = case typ of
    Pointer _ -> PY.call (exprReader typ) [exp]
    Tuple2 _ _ -> PY.call (exprReader typ) [exp]
    Result{} -> PY.call (exprReader typ) [exp]
    Maybe _ -> PY.call (exprReader typ) [exp]
    _ -> exp

argExpr :: Text -> ValueType -> PY.Expr
argExpr var typ =
  case typ of
    Pointer _ -> PY.var var `PY.dot` "ptr"
    ImplicitTolerance -> PY.var var `PY.or` PY.var "global_tolerance"
    _ -> PY.var var

cType :: ValueType -> PY.Expr
cType typ =
  case typ of
    ImplicitTolerance -> PY.var "c_double"
    Pointer _ -> PY.var "c_void_p"
    Float -> PY.var "c_double"
    Boolean -> PY.var "c_bool"
    Maybe _ -> PY.var "c_void_p"
    Result{} -> PY.var "c_void_p"
    Tuple2 _ _ -> PY.var "c_void_p"

pyType :: ValueType -> PY.Expr
pyType typ =
  case typ of
    ImplicitTolerance -> PY.subscript (PY.var "Optional") [PY.var "float"]
    Pointer name -> PY.var name
    Float -> PY.var "float"
    Boolean -> PY.var "bool"
    Maybe nestedTyp -> PY.subscript (PY.var "Optional") [pyType nestedTyp]
    Result _ _ val -> pyType val -- we throw an exception in case of an err
    Tuple2 val1 val2 -> PY.subscript (PY.var "Tuple") [pyType val1, pyType val2]

main :: IO ()
main = do
  let pythonCode = PY.prettyStatements (setup ++ api openSolidAPI)
  Prelude.putStrLn (Text.toChars pythonCode)
