module OpenSolidAPI
  ( genreateForeignFunctions
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

genreateForeignFunctions :: TH.Q (List TH.Dec)
genreateForeignFunctions = Internal.ffi classes

openSolidAPI :: Api
openSolidAPI = $(Internal.api classes)
