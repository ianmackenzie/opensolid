module OpenSolid.CompiledFunction (CompiledFunction) where

import OpenSolid.Prelude

type role CompiledFunction nominal nominal nominal nominal

type CompiledFunction :: Type -> Type -> Type -> Type -> Type
data CompiledFunction inputValue outputValue inputBounds outputBounds
