module OpenSolid.PbrMaterial
  ( PbrMaterial
  , baseColor
  , roughness
  , metallic
  , metal
  , aluminum
  , iron
  , chromium
  , brass
  , copper
  , gold
  , nickel
  , silver
  , titanium
  , nonmetal
  , with
  )
where

import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

-- | A metallic-roughness material used for physically-based rendering.
data PbrMaterial = PbrMaterial
  { baseColor :: Color
  , roughness :: Number
  , metallic :: Number
  }
  deriving (Show)

instance FFI PbrMaterial where
  representation = FFI.classRepresentation "PbrMaterial"

baseColor :: PbrMaterial -> Color
baseColor material = material.baseColor

roughness :: PbrMaterial -> Number
roughness material = material.roughness

metallic :: PbrMaterial -> Number
metallic material = material.metallic

-- | Create a metallic material with the given color and roughness.
metal :: ("baseColor" ::: Color, "roughness" ::: Number) -> PbrMaterial
metal params = with {baseColor = params.baseColor, roughness = params.roughness, metallic = 1.0}

-- | Create an aluminum material with the given roughness.
aluminum :: "roughness" ::: Number -> PbrMaterial
aluminum = metal {baseColor = Color.rgb1 0.960 0.961 0.964}

-- | Create a brass material with the given roughness.
brass :: "roughness" ::: Number -> PbrMaterial
brass = metal {baseColor = Color.rgb1 0.949 0.901 0.690}

-- | Create a chromium material with the given roughness.
chromium :: "roughness" ::: Number -> PbrMaterial
chromium = metal {baseColor = Color.rgb1 0.820 0.827 0.834}

-- | Create a copper material with the given roughness.
copper :: "roughness" ::: Number -> PbrMaterial
copper = metal {baseColor = Color.rgb1 0.967 0.866 0.738}

-- | Create a gold material with the given roughness.
gold :: "roughness" ::: Number -> PbrMaterial
gold = metal {baseColor = Color.rgb1 0.975 0.894 0.645}

-- | Create an iron material with the given roughness.
iron :: "roughness" ::: Number -> PbrMaterial
iron = metal {baseColor = Color.rgb1 0.755 0.743 0.733}

-- | Create a nickel material with the given roughness.
nickel :: "roughness" ::: Number -> PbrMaterial
nickel = metal {baseColor = Color.rgb1 0.826 0.804 0.762}

-- | Create a silver material with the given roughness.
silver :: "roughness" ::: Number -> PbrMaterial
silver = metal {baseColor = Color.rgb1 0.983 0.977 0.965}

-- | Create a titanium material with the given roughness.
titanium :: "roughness" ::: Number -> PbrMaterial
titanium = metal {baseColor = Color.rgb1 0.807 0.787 0.764}

-- | Create a non-metallic material with the given color and roughness.
nonmetal :: ("baseColor" ::: Color, "roughness" ::: Number) -> PbrMaterial
nonmetal params = with {baseColor = params.baseColor, roughness = params.roughness, metallic = 0.0}

-- | Create a material with the given base color, metallic factor and roughness.
with ::
  ( "baseColor" ::: Color
  , "roughness" ::: Number
  , "metallic" ::: Number
  ) ->
  PbrMaterial
with params =
  PbrMaterial
    { baseColor = params.baseColor
    , metallic = params.metallic
    , roughness = params.roughness
    }
