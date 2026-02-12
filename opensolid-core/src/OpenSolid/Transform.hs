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
  Rigid . Rigid = Rigid

instance Composition Orthonormal Rigid Orthonormal where
  Orthonormal . Rigid = Orthonormal

instance Composition Uniform Rigid Uniform where
  Uniform . Rigid = Uniform

instance Composition Affine Rigid Affine where
  Affine . Rigid = Affine

instance Composition Rigid Orthonormal Orthonormal where
  Rigid . Orthonormal = Orthonormal

instance Composition Orthonormal Orthonormal Orthonormal where
  Orthonormal . Orthonormal = Orthonormal

instance Composition Uniform Orthonormal Uniform where
  Uniform . Orthonormal = Uniform

instance Composition Affine Orthonormal Affine where
  Affine . Orthonormal = Affine

instance Composition Rigid Uniform Uniform where
  Rigid . Uniform = Uniform

instance Composition Orthonormal Uniform Uniform where
  Orthonormal . Uniform = Uniform

instance Composition Uniform Uniform Uniform where
  Uniform . Uniform = Uniform

instance Composition Affine Uniform Affine where
  Affine . Uniform = Affine

instance Composition Rigid Affine Affine where
  Rigid . Affine = Affine

instance Composition Orthonormal Affine Affine where
  Orthonormal . Affine = Affine

instance Composition Uniform Affine Affine where
  Uniform . Affine = Affine

instance Composition Affine Affine Affine where
  Affine . Affine = Affine
