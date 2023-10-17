module Main (main) where

import Data.String (String, fromString)
import Language.Python.Common hiding (Class, Float, (<>))
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
  ( IO
  , Maybe (..)
  , concatMap
  , fromInteger
  , map
  , putStrLn
  , uncurry
  , (++)
  , (<>)
  )

-- Define the imports and load the ffi lib
setup :: List (Statement ())
setup =
  [ FromImport
      (ImportRelative 0 (Just [Ident "__future__" ()]) ())
      (FromItems [FromItem (Ident "annotations" ()) Nothing ()] ())
      ()
  , Import [ImportItem [Ident "platform" ()] Nothing ()] ()
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
