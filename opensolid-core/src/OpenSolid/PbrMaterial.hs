module OpenSolid.PbrMaterial
  ( PbrMaterial (baseColor, roughness, metallic)
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
  , custom
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
  , roughness :: Float
  , metallic :: Float
  }

instance FFI PbrMaterial where
  representation = FFI.classRepresentation "PbrMaterial"

-- | Create a metallic material with the given color and roughness.
metal :: Color -> "roughness" ::: Float -> PbrMaterial
metal baseColor (Field roughness) = PbrMaterial{baseColor, metallic = 1.0, roughness}

-- | Create an aluminum material with the given roughness.
aluminum :: "roughness" ::: Float -> PbrMaterial
aluminum = metal (Color.rgbFloat 0.960 0.961 0.964)

-- | Create a brass material with the given roughness.
brass :: "roughness" ::: Float -> PbrMaterial
brass = metal (Color.rgbFloat 0.949 0.901 0.690)

-- | Create a chromium material with the given roughness.
chromium :: "roughness" ::: Float -> PbrMaterial
chromium = metal (Color.rgbFloat 0.820 0.827 0.834)

-- | Create a copper material with the given roughness.
copper :: "roughness" ::: Float -> PbrMaterial
copper = metal (Color.rgbFloat 0.967 0.866 0.738)

-- | Create a gold material with the given roughness.
gold :: "roughness" ::: Float -> PbrMaterial
gold = metal (Color.rgbFloat 0.975 0.894 0.645)

-- | Create an iron material with the given roughness.
iron :: "roughness" ::: Float -> PbrMaterial
iron = metal (Color.rgbFloat 0.755 0.743 0.733)

-- | Create a nickel material with the given roughness.
nickel :: "roughness" ::: Float -> PbrMaterial
nickel = metal (Color.rgbFloat 0.826 0.804 0.762)

-- | Create a silver material with the given roughness.
silver :: "roughness" ::: Float -> PbrMaterial
silver = metal (Color.rgbFloat 0.983 0.977 0.965)

-- | Create a titanium material with the given roughness.
titanium :: "roughness" ::: Float -> PbrMaterial
titanium = metal (Color.rgbFloat 0.807 0.787 0.764)

-- | Create a non-metallic material with the given color and roughness.
nonmetal :: Color -> "roughness" ::: Float -> PbrMaterial
nonmetal baseColor (Field roughness) = PbrMaterial{baseColor, roughness, metallic = 0.0}

-- | Create a material with the given base color, metallic factor and roughness.
custom :: Color -> ("metallic" ::: Float, "roughness" ::: Float) -> PbrMaterial
custom baseColor (Field metallic, Field roughness) = PbrMaterial{baseColor, metallic, roughness}
