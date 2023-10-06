module OpenSolidFFI () where

import Data.String (fromString)
import Foreign.StablePtr (freeStablePtr)
import Language.Haskell.TH qualified as TH
import Prelude (return, (>>=))

-- Generate 'foreign export' declarations using Template Haskell
$( do
    functionType <- TH.reifyType 'freeStablePtr
    return [TH.ForeignD (TH.ExportF TH.CCall "opensolid_free" 'freeStablePtr functionType)]
 )
