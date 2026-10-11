module OpenSolid.API.Docs (docs) where

import Language.Haskell.TH qualified as TH
import OpenSolid.Prelude
import Prelude qualified

docs :: TH.Name -> TH.Q TH.Exp
docs name = do
  maybeString <- TH.getDoc (TH.DeclDoc name)
  case maybeString of
    Just docstring -> Prelude.return (TH.LitE (TH.StringL docstring))
    Nothing -> Prelude.fail (string "Expected a docstring")
