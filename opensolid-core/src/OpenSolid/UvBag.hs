module OpenSolid.UvBag
  ( UvBag
  , empty
  , pack
  , toList
  , toListOf
  , uniqueItems
  )
where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bounded (Bounded)
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)

type UvBag item = Bag UvBounds item

empty :: UvBag item
empty = Bag.empty

pack :: Bounded item UvBounds => List item -> UvBag item
pack = Bag.pack

toList :: UvBag item -> List item
toList = Bag.toList

toListOf :: (a -> b) -> UvBag a -> List b
toListOf = Bag.toListOf

uniqueItems :: ApproximateEquality item Unitless => UvBag item -> List item
uniqueItems = unitless Bag.uniqueItems
