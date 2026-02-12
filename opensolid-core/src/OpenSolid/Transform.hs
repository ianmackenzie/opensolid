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

instance Composition Orthonormal Rigid Orthonormal where
  Orthonormal `compose` Rigid = Orthonormal

instance Composition Uniform Rigid Uniform where
  Uniform `compose` Rigid = Uniform

instance Composition Affine Rigid Affine where
  Affine `compose` Rigid = Affine

instance Composition Rigid Orthonormal Orthonormal where
  Rigid `compose` Orthonormal = Orthonormal

instance Composition Orthonormal Orthonormal Orthonormal where
  Orthonormal `compose` Orthonormal = Orthonormal

instance Composition Uniform Orthonormal Uniform where
  Uniform `compose` Orthonormal = Uniform

instance Composition Affine Orthonormal Affine where
  Affine `compose` Orthonormal = Affine

instance Composition Rigid Uniform Uniform where
  Rigid `compose` Uniform = Uniform

instance Composition Orthonormal Uniform Uniform where
  Orthonormal `compose` Uniform = Uniform

instance Composition Uniform Uniform Uniform where
  Uniform `compose` Uniform = Uniform

instance Composition Affine Uniform Affine where
  Affine `compose` Uniform = Affine

instance Composition Rigid Affine Affine where
  Rigid `compose` Affine = Affine

instance Composition Orthonormal Affine Affine where
  Orthonormal `compose` Affine = Affine

instance Composition Uniform Affine Affine where
  Uniform `compose` Affine = Affine

instance Composition Affine Affine Affine where
  Affine `compose` Affine = Affine
