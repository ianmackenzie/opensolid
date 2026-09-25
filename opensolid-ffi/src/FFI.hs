-- Avoid errors when running Fourmolu
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FFI (generateExports) where

import Language.Haskell.TH qualified as TH
import OpenSolid.API qualified as API
import OpenSolid.API.Function qualified as API.Function
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Prelude qualified

functionArray :: Array FFI.Function
functionArray = case API.functions of
  [] -> error "API somehow has no functions"
  NonEmpty nonEmpty -> Array.fromNonEmpty (NonEmpty.map API.Function.invoke nonEmpty)

invoke :: Int -> FFI.Function
invoke functionIndex = (functionArray @ functionIndex)

generateExports :: TH.Q (List TH.Dec)
generateExports =
  List.indexed (List.map API.Function.ffiName API.functions)
    & Prelude.traverse generateExport
    & Prelude.fmap List.concat

generateExport :: (Int, Text) -> TH.Q (List TH.Dec)
generateExport (index, name) = do
  let nameString = Text.unpack name
  let thName = TH.mkName nameString
  functionTypeAliasInfo <- TH.reify ''FFI.Function
  foreignFunctionType <-
    case functionTypeAliasInfo of
      TH.TyConI (TH.TySynD _ _ typ) -> Prelude.return typ
      _ -> Prelude.fail "Function type alias has unexpected Template Haskell representation"
  let indexLiteral = TH.IntegerL (fromIntegral index)
  let functionBody = TH.NormalB (TH.AppE (TH.VarE 'invoke) (TH.LitE indexLiteral))
  Prelude.return
    [ TH.ForeignD (TH.ExportF TH.CCall nameString thName foreignFunctionType)
    , TH.SigD thName foreignFunctionType
    , TH.FunD thName [TH.Clause [] functionBody []]
    ]
