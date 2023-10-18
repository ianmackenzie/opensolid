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
   in ( PY.cType libName (map (\(_, typ) -> cType typ) pyArgs) (cType retType)
          ++ case kind of
            Static ->
              PY.staticmethod
                pyName
                (map (\(arg, typ) -> (arg, Just (pyType typ))) pyArgs)
                (fromPtr retType (PY.call libName (map (uncurry toPtr) pyArgs)))
                (pyType retType)
            Method ->
              let argsWithoutLast = removeLast pyArgs
               in PY.method
                    pyName
                    (("self", Nothing) : map (\(arg, typ) -> (arg, Just (pyType typ))) argsWithoutLast)
                    (fromPtr retType (PY.call libName (map (uncurry toPtr) argsWithoutLast ++ [PY.var "self.ptr"])))
                    (pyType retType)
      )

removeLast :: List a -> List a
removeLast [] = []
removeLast [_] = []
removeLast (h : t) = h : removeLast t

fromPtr :: ValueType -> Expr () -> Expr ()
fromPtr typ exp =
  case typ of
    Pointer p -> PY.call p [exp]
    _ -> exp

toPtr :: String -> ValueType -> Expr ()
toPtr var typ =
  case typ of
    Pointer _ -> Dot (PY.var var) (Ident "ptr" ()) ()
    _ -> PY.var var

cType :: ValueType -> String
cType typ =
  case typ of
    Pointer _ -> "c_void_p"
    Float -> "c_double"
    Boolean -> "c_bool"

pyType :: ValueType -> String
pyType typ =
  case typ of
    Pointer name -> name
    Float -> "float"
    Boolean -> "bool"

main :: IO ()
main = do
  let pythonCode = prettyText (Module (setup ++ api openSolidAPI))
  putStrLn pythonCode
