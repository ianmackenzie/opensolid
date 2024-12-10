module API.Docs (docs) where

import Language.Haskell.TH qualified as TH
import OpenSolid
import Text qualified
import Prelude qualified

docs :: TH.Name -> TH.Q TH.Exp
docs name = Prelude.do
  maybeString <- TH.getDoc (TH.DeclDoc name)
  case maybeString of
    Just string -> Prelude.return (TH.LitE (TH.StringL string))
    Nothing -> Prelude.fail (Text.unpack "Expected a docstring")
