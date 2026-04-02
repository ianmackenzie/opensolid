module OpenSolid.SearchTree
  ( SearchTree
  , build
  , pairwise
  , domainBounds
  , value
  , children
  )
where

import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.SearchDomain (SearchDomain)
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units

data SearchTree bounds value where
  SearchTree ::
    { domainBounds :: bounds
    , value :: value
    , children :: ~(List (SearchTree bounds value))
    } ->
    SearchTree bounds value

instance
  forall bounds value units.
  HasUnits value units =>
  HasUnits (SearchTree bounds value) units

instance
  (bounds1 ~ bounds2, Units.Coercion value1 value2) =>
  Units.Coercion (SearchTree bounds1 value1) (SearchTree bounds2 value2)
  where
  coerce searchTree =
    SearchTree
      { domainBounds = searchTree.domainBounds
      , value = Units.coerce searchTree.value
      , children = List.map Units.coerce searchTree.children
      }

domainBounds :: forall bounds value. SearchTree bounds value -> bounds
domainBounds = (.domainBounds)

value :: forall bounds value. SearchTree bounds value -> value
value = (.value)

children :: forall bounds value. SearchTree bounds value -> List (SearchTree bounds value)
children = (.children)

build :: forall bounds value. (bounds -> value) -> SearchDomain bounds -> SearchTree bounds value
build function searchDomain = do
  let bounds = SearchDomain.bounds searchDomain
  let subdomains = SearchDomain.children searchDomain
  SearchTree bounds (function bounds) (List.map (build function) subdomains)

pairwise ::
  (value1 -> value2 -> value3) ->
  SearchTree bounds1 value1 ->
  SearchTree bounds2 value2 ->
  SearchTree (bounds1, bounds2) value3
pairwise function tree1 tree2 = do
  let SearchTree domainBounds1 value1 children1 = tree1
  let SearchTree domainBounds2 value2 children2 = tree2
  SearchTree (domainBounds1, domainBounds2) (function value1 value2) $
    [pairwise function child1 child2 | child1 <- children1, child2 <- children2]
