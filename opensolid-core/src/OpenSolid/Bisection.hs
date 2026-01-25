{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Bisection
  ( Domain (contains, overlaps, interiorOf, isInterior, domain)
  , Subdomain
  , bounds
  , bisect
  , map2
  , map3
  , Size (..)
  , Classification (..)
  , search
  , classify
  , Tree (Tree)
  , tree
  )
where

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.UvBounds (UvBounds, pattern UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)

class Domain bounds value | bounds -> value, value -> bounds where
  contains :: bounds -> bounds -> Bool
  overlaps :: bounds -> bounds -> Bool
  interiorOf :: bounds -> bounds
  isInterior :: value -> bounds -> Bool
  domain :: Subdomain bounds

data Subdomain bounds where
  Split :: Interval Unitless -> Subdomain (Interval Unitless)
  Shrink :: Interval Unitless -> Subdomain (Interval Unitless)
  Map2 ::
    (bounds1 -> bounds2 -> bounds3) ->
    Subdomain bounds1 ->
    Subdomain bounds2 ->
    Subdomain bounds3
  Map3 ::
    (bounds1 -> bounds2 -> bounds3 -> bounds4) ->
    Subdomain bounds1 ->
    Subdomain bounds2 ->
    Subdomain bounds3 ->
    Subdomain bounds4

bounds :: Subdomain bounds -> bounds
bounds (Split interval) = interval
bounds (Shrink interval) = interval
bounds (Map2 function subdomain1 subdomain2) =
  function (bounds subdomain1) (bounds subdomain2)
bounds (Map3 function subdomain1 subdomain2 subdomain3) =
  function (bounds subdomain1) (bounds subdomain2) (bounds subdomain3)

data InfiniteRecursion = InfiniteRecursion deriving (Show, Exception)

bisect :: Subdomain bounds -> List (Subdomain bounds)
bisect (Split interval) = do
  let (# low, lowMid, mid, highMid, high #) = quadrisectInterval# interval
  [Split (Interval low mid), Shrink (Interval lowMid highMid), Split (Interval mid high)]
bisect (Shrink interval) = do
  let (# _, lowMid, _, highMid, _ #) = quadrisectInterval# interval
  [Shrink (Interval lowMid highMid)]
bisect (Map2 function subdomain1 subdomain2) =
  [Map2 function child1 child2 | child1 <- bisect subdomain1, child2 <- bisect subdomain2]
bisect (Map3 function subdomain1 subdomain2 subdomain3) =
  [ Map3 function child1 child2 child3
  | child1 <- bisect subdomain1
  , child2 <- bisect subdomain2
  , child3 <- bisect subdomain3
  ]

map2 ::
  (bounds1 -> bounds2 -> bounds3) ->
  Subdomain bounds1 ->
  Subdomain bounds2 ->
  Subdomain bounds3
map2 = Map2

map3 ::
  (bounds1 -> bounds2 -> bounds3 -> bounds4) ->
  Subdomain bounds1 ->
  Subdomain bounds2 ->
  Subdomain bounds3 ->
  Subdomain bounds4
map3 = Map3

quadrisectInterval# :: Interval Unitless -> (# Number, Number, Number, Number, Number #)
quadrisectInterval# (Interval low high) = do
  let mid = Number.midpoint low high
  let lowMid = Number.midpoint low mid
  let highMid = Number.midpoint mid high
  if low < lowMid && lowMid < mid && mid < highMid && highMid < high
    then (# low, lowMid, mid, highMid, high #)
    else throw InfiniteRecursion

instance Domain (Interval Unitless) Number where
  contains = Interval.contains
  overlaps interval1 interval2 = Interval.overlap interval1 interval2 > Quantity.zero
  interiorOf interval = do
    let (# interiorLow, interiorHigh #) = intervalInterior# interval
    Interval interiorLow interiorHigh
  isInterior value interval = do
    let (# interiorLow, interiorHigh #) = intervalInterior# interval
    interiorLow <= value && value <= interiorHigh
  domain = Split Interval.unit

intervalInterior# :: Interval Unitless -> (# Number, Number #)
intervalInterior# (Interval exteriorLow exteriorHigh) = do
  let margin = 0.125 *. (exteriorHigh .-. exteriorLow)
  let interiorLow = if exteriorLow == 0 then 0 else exteriorLow .+. margin
  let interiorHigh = if exteriorHigh == 1 then 1 else exteriorHigh .-. margin
  (# interiorLow, interiorHigh #)

instance Domain UvBounds UvPoint where
  contains = Bounds2D.contains
  overlaps (Bounds2D x1 y1) (Bounds2D x2 y2) = overlaps x1 x2 && overlaps y1 y2
  interiorOf (UvBounds uBounds vBounds) =
    UvBounds (interiorOf uBounds) (interiorOf vBounds)
  isInterior (UvPoint uValue vValue) (UvBounds uBounds vBounds) =
    isInterior uValue uBounds && isInterior vValue vBounds
  domain = Map2 UvBounds domain domain

instance
  (Domain bounds1 value1, Domain bounds2 value2) =>
  Domain (bounds1, bounds2) (value1, value2)
  where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2
  overlaps (b1, b2) (a1, a2) = overlaps b1 a1 && overlaps b2 a2
  interiorOf (b1, b2) = (interiorOf b1, interiorOf b2)
  isInterior (v1, v2) (b1, b2) = isInterior v1 b1 && isInterior v2 b2
  domain = Map2 (,) domain domain

search :: Domain bounds value => (bounds -> Fuzzy (Maybe solution)) -> List (bounds, solution)
search callback = searchImpl callback [] [domain]

searchImpl ::
  Domain bounds value =>
  (bounds -> Fuzzy (Maybe solution)) ->
  List (bounds, solution) ->
  List (Subdomain bounds) ->
  List (bounds, solution)
searchImpl _ accumulated [] = accumulated
searchImpl callback accumulated subdomains = do
  let solutionsAndChildren = List.map (visit callback accumulated) subdomains
  let (solutions, children) = List.unzip2 solutionsAndChildren
  searchImpl callback (List.concat (accumulated : solutions)) (List.concat children)

visit ::
  Domain bounds value =>
  (bounds -> Fuzzy (Maybe solution)) ->
  List (bounds, solution) ->
  Subdomain bounds ->
  (List (bounds, solution), List (Subdomain bounds))
visit callback accumulated subdomain = do
  let subdomainBounds = bounds subdomain
  if List.anySatisfy (contains subdomainBounds . Pair.first) accumulated
    then ([], [])
    else case callback subdomainBounds of
      Resolved Nothing -> ([], [])
      Resolved (Just solution) -> ([(subdomainBounds, solution)], [])
      Unresolved -> ([], bisect subdomain)

data Size = Small | Large deriving (Eq, Ord, Show)

data Classification = Entire | Interior | Start Size | End Size

classify :: Interval Unitless -> Classification
classify (Interval 0 1) = Entire
classify (Interval 0 t) = Start (if t <= Desingularization.t0 then Small else Large)
classify (Interval t 1) = End (if t >= Desingularization.t1 then Small else Large)
classify (Interval _ _) = Interior

data Tree a = Tree a ~(List (Tree a))

tree :: Domain bounds value => (bounds -> a) -> Tree a
tree callback = buildTree callback domain

buildTree :: Domain bounds value => (bounds -> a) -> Subdomain bounds -> Tree a
buildTree callback subdomain = do
  let value = callback (bounds subdomain)
  let children = List.map (buildTree callback) (bisect subdomain)
  Tree value children
