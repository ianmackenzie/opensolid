module Expression1d.Root (Root (..)) where

import OpenSolid

data Root = Root {value :: !Float, order :: !Int}
