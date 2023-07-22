module Generic
  ( Zero (zero)
  )
where

class Zero a where
  zero :: a
