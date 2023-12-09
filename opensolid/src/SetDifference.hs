module SetDifference (SetDifference ((\\))) where

import Basics
import Data.List qualified

class SetDifference a b c | a b -> c where
  (\\) :: a -> b -> c

instance (Eq a) => SetDifference (List a) (List a) (List a) where
  (\\) = (Data.List.\\)
