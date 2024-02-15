module Main (main) where

import List qualified
import Maybe qualified
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
import String qualified
import System.Exit qualified as SE
import System.IO qualified as SIO
import System.Process qualified as SP
import Task qualified

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
    , String.multiline
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
    , String.multiline
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
    , String.multiline
        [ "def maybe_reader(read_success: Callable[[c_void_p], A]) -> Callable[[c_void_p], Optional[A]]:"
        , "    return lambda ptr: read_success(ptr) if ptr else None"
        ]
    , String.multiline
        [ "class Tuple2Structure(Structure):"
        , "    _fields_ = [('ptr1', c_void_p), ('ptr2', c_void_p)]"
        ]
    , String.multiline
        [ "def tuple2_reader(read_a: Callable[[c_void_p], A], read_b: Callable[[c_void_p], B]) -> Callable[[c_void_p], Tuple[A, B]]:"
        , "    def read(ptr: c_void_p) -> Tuple[A, B]:"
        , "        tuple2_struct = cast(ptr, POINTER(Tuple2Structure)).contents"
        , "        res = (read_a(tuple2_struct.ptr1), read_b(tuple2_struct.ptr2))"
        , "        lib.opensolid_free(ptr)"
        , "        return res"
        , "    return read"
        ]
    , String.multiline
        [ "class ResultStructure(Structure):"
        , "    _fields_ = [('ptr', c_void_p), ('tag', c_int8)]"
        ]
    , String.multiline
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
    , String.multiline
        [ "def read_float(ptr: c_void_p) -> float:"
        , "    val = float(cast(ptr, POINTER(c_double)).contents.value)"
        , "    lib.opensolid_free(ptr)"
        , "    return val"
        ]
    , String.multiline
        [ "def read_bool(ptr: c_void_p) -> bool:"
        , "    val = bool(cast(ptr, POINTER(c_bool)).contents.value)"
        , "    lib.opensolid_free(ptr)"
        , "    return val"
        ]
    , String.multiline
        [ "def read_tolerance(tolerance: Optional[float]) -> float:"
        , "    tolerance = tolerance or global_tolerance"
        , "    if tolerance is None:"
        , "        raise Exception('Tolerance is not set')"
        , "    return tolerance"
        ]
    ]

api :: Api -> List PY.Statement
api (Api classes) =
  List.map apiClass classes

apiClass :: Class -> PY.Statement
apiClass (Class clsName representationProps errorClasses functions) =
  PY.cls clsName [] <|
    List.concat
      [ [constructor, destructor, representation]
      , List.collect apiFunction functions
      , List.collect (apiException clsName) errorClasses
      ]
 where
  constructor =
    PY.def "__init__" [selfPyArg, ("ptr", Just (PY.var "c_void_p"), Nothing)] Nothing <|
      [PY.set (PY.var "self" `PY.dot` "ptr") (PY.var "ptr")]

  destructor =
    PY.def "__del__" [selfPyArg] Nothing <|
      [PY.stmtExpr (PY.call (PY.var "lib" `PY.dot` "opensolid_free_stable") [PY.var "self" `PY.dot` "ptr"])]

  representation =
    PY.def "__repr__" [selfPyArg] (Just (PY.var "str")) <|
      [ PY.returnStatement
          ( List.foldLeft
              PY.plus
              (PY.string (clsName ++ "("))
              ( List.intersperse
                  (PY.string ", ")
                  (List.map (\prop -> PY.call (PY.var "str") [PY.call (PY.var "self" `PY.dot` prop) []]) representationProps)
              )
              `PY.plus` PY.string ")"
          )
      ]

apiFunction :: Function -> List PY.Statement
apiFunction (Function kind ffiName pyName args retType) =
  [ -- lib.opensolid_point2d_xy.argtypes = [c_double, c_double]
    PY.set (libName `PY.dot` "argtypes") (PY.list (List.map (\(_, typ) -> cType typ) args))
  , -- lib.opensolid_point2d_xy.restype = c_void_p
    PY.set (libName `PY.dot` "restype") (cType retType)
  , case kind of
      Static ->
        PY.staticmethod <|
          PY.def
            pyName
            -- "tolerance" is the last arg
            (pyArgs (appendMaybe tolerance argsWithoutTolerance))
            (pyType retType)
            (fnBody retType libName (ffiArgExprs args))
      Method ->
        PY.def
          pyName
          -- "self" is the first, "tolerance" is the last arg
          (selfPyArg : pyArgs (appendMaybe tolerance (removeLast argsWithoutTolerance)))
          (pyType retType)
          (fnBody retType libName (ffiArgExprs args))
  ]
 where
  libName = PY.var "lib" `PY.dot` ffiName
  (tolerance, argsWithoutTolerance) = takeTolerance args

  appendMaybe (Just tol) list = list ++ [tol]
  appendMaybe Nothing list = list

  removeLast [] = []
  removeLast [_] = []
  removeLast (h : t) = h : removeLast t

  takeTolerance ((tol, ImplicitTolerance) : rest) = (Just (tol, ImplicitTolerance), rest)
  takeTolerance rest = (Nothing, rest)

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
    Self -> PY.var "c_void_p"

pyArgs :: List (String, ValueType) -> List (String, Maybe PY.Expr, Maybe PY.Expr)
pyArgs = List.map pyArg
 where
  pyArg (name, typ) =
    let defaultValue = case typ of
          ImplicitTolerance -> Just (PY.var "None")
          _ -> Nothing
     in (name, pyType typ, defaultValue)

selfPyArg :: (String, Maybe PY.Expr, Maybe PY.Expr)
selfPyArg = ("self", Nothing, Nothing)

pyType :: ValueType -> Maybe PY.Expr
pyType typ =
  case typ of
    ImplicitTolerance -> Just (PY.subscript (PY.var "Optional") [PY.var "float"])
    Pointer name -> Just (PY.var name)
    Float -> Just (PY.var "float")
    Boolean -> Just (PY.var "bool")
    Maybe nestedTyp -> Just (PY.subscript (PY.var "Optional") (Maybe.collect pyType [nestedTyp]))
    Result _ _ val -> pyType val -- we throw an exception in case of an err
    Tuple2 val1 val2 -> Just (PY.subscript (PY.var "Tuple") (Maybe.collect pyType [val1, val2]))
    Self -> Nothing

fnBody :: ValueType -> PY.Expr -> List PY.Expr -> [PY.Statement]
fnBody typ ffiFunc args =
  [PY.returnStatement expression]
 where
  fnCall = PY.call ffiFunc args
  expression = case typ of
    Pointer _ -> PY.call (exprReader typ) [fnCall]
    Tuple2 _ _ -> PY.call (exprReader typ) [fnCall]
    Result{} -> PY.call (exprReader typ) [fnCall]
    Maybe _ -> PY.call (exprReader typ) [fnCall]
    Self -> PY.var "self" `PY.dot` "ptr"
    _ -> fnCall

-- Compose the readers that decode the result value of the c function
-- stable pointers are wrapped in the python class
-- tuples and results are transformed into python values and
-- the c pointers are freed
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
    Self -> PY.var "self" `PY.dot` "ptr"

ffiArgExprs :: List (String, ValueType) -> List PY.Expr
ffiArgExprs = List.map ffiArgExpr
 where
  ffiArgExpr (var, typ) =
    case typ of
      Self -> PY.var "self" `PY.dot` "ptr"
      Pointer _ -> PY.var var `PY.dot` "ptr"
      ImplicitTolerance -> PY.call (PY.var "read_tolerance") [PY.var var]
      _ -> PY.var var

apiException :: String -> ExceptionClass -> List PY.Statement
apiException mod (ExceptionClass name [(tag, constructorName, Nothing)])
  | name == constructorName =
      let retTyp = PY.var mod `PY.dot` name
       in [ PY.cls name [PY.var "Exception"] <|
              [ PY.staticmethod
                  ( PY.def
                      "from_tag"
                      [ ("tag", Just (PY.var "c_int8"), Nothing)
                      , ("ptr", Just (PY.var "c_void_p"), Nothing)
                      ]
                      (Just retTyp)
                      [ PY.conditional
                          [
                            ( PY.var "tag" `PY.eq` PY.int tag
                            , [PY.returnStatement (PY.call retTyp [])]
                            )
                          ]
                          (PY.literalStatement "raise ValueError('Unkown exception tag ' + str(tag))")
                      ]
                  )
              ]
          ]
-- TODO: support exceptions with mutiple cases and pointers
apiException _ ex = internalError (show ex)

main :: IO ()
main =
  Task.toIO <| Task.do
    let pythonCode = PY.prettyStatements (setup ++ api openSolidAPI)
    let ruffCmd = SP.proc "ruff" ["format", "--stdin-filename", "opensolid.py", "--quiet"]
    (Just stdinHandle, _, _, process) <- Task.fromIO (SP.createProcess ruffCmd{SP.std_in = SP.CreatePipe})
    Task.fromIO (SIO.hPutStr stdinHandle pythonCode)
    Task.fromIO (SIO.hClose stdinHandle)
    ruffExitCode <- Task.fromIO (SP.waitForProcess process)
    if ruffExitCode == SE.ExitSuccess
      then return ()
      else fail "Error when running Ruff"
