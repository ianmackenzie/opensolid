-- Avoid errors when running Fourmolu
{-# LANGUAGE GHC2021 #-}

module FFI (generateExports) where

import API qualified
import Foreign (Ptr)
import Language.Haskell.TH qualified as TH
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Prelude qualified

type Function = Ptr () -> Ptr () -> IO ()

functionArray :: Array Function
functionArray = case API.functions of
  [] -> internalError "API somehow has no functions"
  NonEmpty nonEmpty -> Array.new (NonEmpty.map API.invoke nonEmpty)

invoke :: Int -> Ptr () -> Ptr () -> IO ()
invoke functionIndex = Array.get functionIndex functionArray

generateExports :: TH.Q (List TH.Dec)
generateExports =
  Prelude.fmap List.concat $
    Prelude.traverse generateExport (List.indexed (List.map API.ffiName API.functions))

generateExport :: (Int, Text) -> TH.Q (List TH.Dec)
generateExport (index, name) = Prelude.do
  let nameString = Text.unpack name
  let thName = TH.mkName nameString
  functionTypeAliasInfo <- TH.reify ''Function
  foreignFunctionType <-
    case functionTypeAliasInfo of
      TH.TyConI (TH.TySynD _ _ typ) -> Prelude.return typ
      _ -> Prelude.fail (Text.unpack "Function type alias has unexpected Template Haskell representation")
  let indexLiteral = TH.IntegerL (fromIntegral index)
  let functionBody = TH.NormalB (TH.AppE (TH.VarE 'invoke) (TH.LitE indexLiteral))
  Prelude.return
    [ TH.ForeignD (TH.ExportF TH.CCall nameString thName foreignFunctionType)
    , TH.SigD thName foreignFunctionType
    , TH.FunD thName [TH.Clause [] functionBody []]
    ]
