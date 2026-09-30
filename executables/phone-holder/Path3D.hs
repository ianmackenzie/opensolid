module Path3D (reverse) where

import Edge3D (Edge3D)
import Edge3D qualified
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude

reverse :: NonEmpty (Edge3D space) -> NonEmpty (Edge3D space)
reverse = NonEmpty.reverseMap Edge3D.reverse
