module Generic
  ( Zero (..)
  )
where

class Zero a where
  zero :: a
