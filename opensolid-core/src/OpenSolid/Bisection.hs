module OpenSolid.Bisection
  ( Action
  , return
  , recurse
  , Domain (Domain)
  , parameterDomain
  , curveDomain
  , curvePairDomain
  , surfaceDomain
  , curveSurfaceDomain
  , surfacePairDomain
  , search
  , map2
  , map3
  , map4
  )
where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Error qualified as Error
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.UvBounds (UvBounds)

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error.Message)

data Action solution where
  Return :: Maybe solution -> Action solution
  Recurse :: Action solution

return :: Maybe solution -> Action solution
return = Return

recurse :: Action solution
recurse = Recurse

data Domain bounds = Domain bounds ~(List (Domain bounds))

split :: Bounds Unitless -> Domain (Bounds Unitless)
split bounds = Domain bounds do
  let Bounds low high = bounds
  let mid = Float.midpoint low high
  let lowMid = Float.midpoint low mid
  let highMid = Float.midpoint mid high
  if mid > low && mid < high
    then [split (Bounds low mid), shrink (Bounds lowMid highMid), split (Bounds mid high)]
    else []

shrink :: Bounds Unitless -> Domain (Bounds Unitless)
shrink bounds = Domain bounds do
  let Bounds low high = bounds
  let mid = Float.midpoint low high
  let lowMid = Float.midpoint low mid
  let highMid = Float.midpoint mid high
  [shrink (Bounds lowMid highMid) | lowMid > low && highMid < high]

parameterDomain :: Domain (Bounds Unitless)
parameterDomain = split Bounds.unitInterval

curveDomain :: Domain (Bounds Unitless)
curveDomain = parameterDomain

curvePairDomain :: Domain (Bounds Unitless, Bounds Unitless)
curvePairDomain = map2 (,) curveDomain curveDomain

surfaceDomain :: Domain UvBounds
surfaceDomain = map2 Bounds2d parameterDomain parameterDomain

curveSurfaceDomain :: Domain (Bounds Unitless, UvBounds)
curveSurfaceDomain = map2 (,) curveDomain surfaceDomain

surfacePairDomain :: Domain (UvBounds, UvBounds)
surfacePairDomain = map2 (,) surfaceDomain surfaceDomain

search :: Domain bounds -> (bounds -> Action solution) -> Result InfiniteRecursion (List solution)
search domain callback = searchImpl callback [domain] []

searchImpl ::
  (bounds -> Action solution) ->
  List (Domain bounds) ->
  List solution ->
  Result InfiniteRecursion (List solution)
searchImpl _ [] accumulated = Success accumulated
searchImpl callback (Domain bounds children : rest) accumulated =
  case callback bounds of
    Return Nothing -> searchImpl callback rest accumulated
    Return (Just solution) -> searchImpl callback rest (solution : accumulated)
    Recurse -> case children of
      [] -> Failure InfiniteRecursion
      _ -> accumulated |> searchImpl callback children |> Result.andThen (searchImpl callback rest)

map2 ::
  (bounds1 -> bounds2 -> bounds3) ->
  Domain bounds1 ->
  Domain bounds2 ->
  Domain bounds3
map2 function domain1 domain2 = do
  let Domain bounds1 children1 = domain1
  let Domain bounds2 children2 = domain2
  Domain (function bounds1 bounds2) $
    [ map2 function child1 child2
    | child1 <- children1
    , child2 <- children2
    ]

map3 ::
  (bounds1 -> bounds2 -> bounds3 -> bounds4) ->
  Domain bounds1 ->
  Domain bounds2 ->
  Domain bounds3 ->
  Domain bounds4
map3 function domain1 domain2 domain3 = do
  let Domain bounds1 children1 = domain1
  let Domain bounds2 children2 = domain2
  let Domain bounds3 children3 = domain3
  Domain (function bounds1 bounds2 bounds3) $
    [ map3 function child1 child2 child3
    | child1 <- children1
    , child2 <- children2
    , child3 <- children3
    ]

map4 ::
  (bounds1 -> bounds2 -> bounds3 -> bounds4 -> bounds5) ->
  Domain bounds1 ->
  Domain bounds2 ->
  Domain bounds3 ->
  Domain bounds4 ->
  Domain bounds5
map4 function domain1 domain2 domain3 domain4 = do
  let Domain bounds1 children1 = domain1
  let Domain bounds2 children2 = domain2
  let Domain bounds3 children3 = domain3
  let Domain bounds4 children4 = domain4
  Domain (function bounds1 bounds2 bounds3 bounds4) $
    [ map4 function child1 child2 child3 child4
    | child1 <- children1
    , child2 <- children2
    , child3 <- children3
    , child4 <- children4
    ]
