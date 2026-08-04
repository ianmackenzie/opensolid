module OpenSolid.API.Docs (docs) where

import Language.Haskell.TH qualified as TH
import OpenSolid.Prelude
import Prelude qualified

docs :: TH.Name -> TH.Q TH.Exp
docs name = do
  maybeString <- TH.getDoc (TH.DeclDoc name)
  case maybeString of
    Just string -> Prelude.return (TH.LitE (TH.StringL string))
    Nothing -> Prelude.fail "Expected a docstring"
