module OpenSolidAPI
  ( generateForeignFunctions
  , openSolidAPI
  , Api (..)
  , Class (..)
  , Function (..)
  , FunctionKind (..)
  , ValueType (..)
  , ExceptionClass (..)
  )
where

import All (classes)
import Api
import Internal qualified
import Language.Haskell.TH qualified as TH
import OpenSolid

generateForeignFunctions :: TH.Q (List TH.Dec)
generateForeignFunctions = Internal.ffi classes

openSolidAPI :: Api
openSolidAPI = $(Internal.api classes)
