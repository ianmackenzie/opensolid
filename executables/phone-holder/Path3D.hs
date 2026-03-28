module Path3D
  ( on
  , reverse
  )
where

import Edge2D (Edge2D)
import Edge3D (Edge3D)
import Edge3D qualified
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude

on :: Plane3D space local -> NonEmpty (Edge2D local) -> NonEmpty (Edge3D space)
on plane path2D = NonEmpty.map (Edge3D.on plane) path2D

reverse :: NonEmpty (Edge3D space) -> NonEmpty (Edge3D space)
reverse = NonEmpty.reverseMap Edge3D.reverse
