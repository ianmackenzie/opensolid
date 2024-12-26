module OpenSolid.Polygon2d
  ( Polygon2d
  , outerLoop
  , innerLoops
  , singleLoop
  , withHoles
  )
where

import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Triangle2d (Triangle2d (Triangle2d))
import OpenSolid.Triangle2d qualified as Triangle2d
import OpenSolid.Vertex2d (Vertex2d)

data Polygon2d vertex = Polygon2d
  { outerLoop :: NonEmpty vertex
  , innerLoops :: List (NonEmpty vertex)
  }

singleLoop :: Vertex2d vertex (space @ units) => NonEmpty vertex -> Polygon2d vertex
singleLoop givenOuterLoop = withHoles [] givenOuterLoop

withHoles ::
  Vertex2d vertex (space @ units) =>
  List (NonEmpty vertex) ->
  NonEmpty vertex ->
  Polygon2d vertex
withHoles givenInnerLoops givenOuterLoop =
  Polygon2d
    { outerLoop = makeLoop Positive givenOuterLoop
    , innerLoops = List.map (makeLoop Negative) givenInnerLoops
    }

makeLoop :: Vertex2d vertex (space @ units) => Sign -> NonEmpty vertex -> NonEmpty vertex
makeLoop desiredSign vertices =
  if Qty.sign (counterclockwiseArea' vertices) == desiredSign
    then vertices
    else NonEmpty.reverse vertices

counterclockwiseArea' :: Vertex2d vertex (space @ units) => NonEmpty vertex -> Qty (units :*: units)
counterclockwiseArea' (v0 :| vs) = do
  let triangleSignedArea' v1 v2 = Triangle2d.signedArea' (Triangle2d v0 v1 v2)
  Qty.sum (List.successive triangleSignedArea' vs)
