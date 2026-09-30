module OpenSolid.API.Docs (docs) where

import Language.Haskell.TH qualified as TH
import OpenSolid.Monad qualified as Monad
import OpenSolid.Prelude

docs :: TH.Name -> TH.Q TH.Exp
docs name = do
  maybeString <- TH.getDoc (TH.DeclDoc name)
  case maybeString of
    Just docstring -> Monad.return (TH.LitE (TH.StringL docstring))
    Nothing -> Monad.fail "Expected a docstring"
