module SetDifference (SetDifference ((\\))) where

class SetDifference a b c | a b -> c where
  (\\) :: a -> b -> c
