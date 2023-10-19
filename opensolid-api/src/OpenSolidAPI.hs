module OpenSolidAPI
  ( generateForeignFunctions
  , openSolidAPI
  , Api (..)
  , Class (..)
  , Function (..)
  , FunctionKind (..)
  , ValueType (..)
  )
where

import All (classes)
import Api
import Data.String (fromString)
import Internal qualified
import Language.Haskell.TH qualified as TH
import OpenSolid (List)

generateForeignFunctions :: TH.Q (List TH.Dec)
generateForeignFunctions = Internal.ffi classes

openSolidAPI :: Api
openSolidAPI = $(Internal.api classes)
