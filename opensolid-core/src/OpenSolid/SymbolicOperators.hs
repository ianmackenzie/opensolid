module OpenSolid.SymbolicOperators
  ( type (:+:) ((:+:))
  , type (:-:) ((:-:))
  , type (:*:) ((:*:))
  , type (:/:) ((:/:))
  )
where

import OpenSolid.Bootstrap

data a :+: b = a :+: b deriving (Eq, Show)

infixl 6 :+:

data a :-: b = a :-: b deriving (Eq, Show)

infixl 6 :-:

data a :*: b = a :*: b deriving (Eq, Show)

infixl 7 :*:

data a :/: b = a :/: b deriving (Eq, Show)

infixl 7 :/:
