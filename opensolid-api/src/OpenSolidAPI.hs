module OpenSolidAPI
  ( genreateForeignFunctions
  , openSolidAPI
  , Api (..)
  , Class (..)
  , Function (..)
  , FunctionKind (..)
  , Type (..)
  )
where

import All (classes)
import Api
import Data.String (fromString)
import Internal qualified
import Language.Haskell.TH qualified as TH

genreateForeignFunctions :: TH.Q [TH.Dec]
genreateForeignFunctions = Internal.ffi classes

openSolidAPI :: Api
openSolidAPI = $(Internal.api classes)
