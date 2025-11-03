module OpenSolid.API.Docs (docs) where

import Language.Haskell.TH qualified as TH
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

docs :: TH.Name -> TH.Q TH.Exp
docs name = do
  maybeString <- TH.getDoc (TH.DeclDoc name)
  case maybeString of
    Just string -> return (TH.LitE (TH.StringL string))
    Nothing -> fail (Text.unpack "Expected a docstring")
