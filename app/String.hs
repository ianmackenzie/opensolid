module String (
    concat,
) where

import qualified Data.Text
import OpenSolid

concat :: List String -> String
concat = Data.Text.concat
