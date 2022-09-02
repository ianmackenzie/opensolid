module String (
    concat,
) where

import qualified Data.Text
import OpenSolid

concat :: [String] -> String
concat = Data.Text.concat
