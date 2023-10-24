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
import Codegen qualified
import Internal qualified
import Language.Haskell.TH qualified as TH
import OpenSolid

generateForeignFunctions :: TH.Q (List TH.Dec)
generateForeignFunctions = Codegen.execute (Internal.ffi classes)

openSolidAPI :: Api
openSolidAPI = $(Codegen.execute (Internal.api classes))
