{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module OpenSolid.Scene3d.Material
  ( Material (..)
  , metal
  , nonmetal
  , pbr
  )
where

import OpenSolid.Color (Color)
import OpenSolid.Prelude
import OpenSolid.Units (Meters)
import OpenSolid.Vertex3d qualified as Vertex3d

data Material vertex where
  Pbr ::
    Vertex3d.HasNormal vertex (space @ Meters) =>
    { baseColor :: Color
    , roughness :: Float
    , metallic :: Float
    } ->
    Material vertex

deriving instance Eq (Material vertex)

metal :: Vertex3d.HasNormal vertex (space @ Meters) => Color -> Float -> Material vertex
metal baseColor roughness = pbr baseColor roughness 1.0

nonmetal :: Vertex3d.HasNormal vertex (space @ Meters) => Color -> Float -> Material vertex
nonmetal baseColor roughness = pbr baseColor roughness 0.0

pbr :: Vertex3d.HasNormal vertex (space @ Meters) => Color -> Float -> Float -> Material vertex
pbr = Pbr
