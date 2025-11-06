module OpenSolid.Transform
  ( Rigid
  , Orthonormal
  , Uniform
  , Affine
  , IsRigid
  , IsOrthonormal
  , IsUniform
  )
where

import OpenSolid.Prelude

data Rigid = Rigid deriving (Eq, Show)

data Orthonormal = Orthonormal deriving (Eq, Show)

data Uniform = Uniform deriving (Eq, Show)

data Affine = Affine deriving (Eq, Show)

class IsOrthonormal tag => IsRigid tag

class IsUniform tag => IsOrthonormal tag

class IsUniform tag

instance IsRigid Rigid

instance IsOrthonormal Rigid

instance IsUniform Rigid

instance IsOrthonormal Orthonormal

instance IsUniform Orthonormal

instance IsUniform Uniform

instance Composition Rigid Rigid Rigid where
  Rigid `compose` Rigid = Rigid

instance Composition Rigid Orthonormal Orthonormal where
  Orthonormal `compose` Rigid = Orthonormal

instance Composition Rigid Uniform Uniform where
  Uniform `compose` Rigid = Uniform

instance Composition Rigid Affine Affine where
  Affine `compose` Rigid = Affine

instance Composition Orthonormal Rigid Orthonormal where
  Rigid `compose` Orthonormal = Orthonormal

instance Composition Orthonormal Orthonormal Orthonormal where
  Orthonormal `compose` Orthonormal = Orthonormal

instance Composition Orthonormal Uniform Uniform where
  Uniform `compose` Orthonormal = Uniform

instance Composition Orthonormal Affine Affine where
  Affine `compose` Orthonormal = Affine

instance Composition Uniform Rigid Uniform where
  Rigid `compose` Uniform = Uniform

instance Composition Uniform Orthonormal Uniform where
  Orthonormal `compose` Uniform = Uniform

instance Composition Uniform Uniform Uniform where
  Uniform `compose` Uniform = Uniform

instance Composition Uniform Affine Affine where
  Affine `compose` Uniform = Affine

instance Composition Affine Rigid Affine where
  Rigid `compose` Affine = Affine

instance Composition Affine Orthonormal Affine where
  Orthonormal `compose` Affine = Affine

instance Composition Affine Uniform Affine where
  Uniform `compose` Affine = Affine

instance Composition Affine Affine Affine where
  Affine `compose` Affine = Affine
