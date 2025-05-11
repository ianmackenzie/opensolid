module OpenSolid.Basis3d
  ( Basis3d
  , world
  , rightwardDirection
  , leftwardDirection
  , forwardDirection
  , backwardDirection
  , upwardDirection
  , downwardDirection
  , rightPlaneBasis
  , leftPlaneBasis
  , frontPlaneBasis
  , backPlaneBasis
  , topPlaneBasis
  , bottomPlaneBasis
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Basis3d, Direction3d, PlanarBasis3d)

world :: Basis3d space (Defines space)
rightwardDirection :: Basis3d space defines -> Direction3d space
leftwardDirection :: Basis3d space defines -> Direction3d space
forwardDirection :: Basis3d space defines -> Direction3d space
backwardDirection :: Basis3d space defines -> Direction3d space
upwardDirection :: Basis3d space defines -> Direction3d space
downwardDirection :: Basis3d space defines -> Direction3d space
rightPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
leftPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
frontPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
backPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
topPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
bottomPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
