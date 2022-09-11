module Verdict (Verdict (..)) where

import Prelude (Bool (..), Show)

data Verdict = Definitely Bool | Indeterminate
    deriving (Show)
