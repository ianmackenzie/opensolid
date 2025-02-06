module OpenSolid.Transform2d
  ( translateByImpl
  , translateInImpl
  , translateAlongImpl
  , rotateAroundImpl
  , mirrorAcrossImpl
  , scaleAboutImpl
  , scaleAlongImpl
  , translateByOwnImpl
  , translateInOwnImpl
  , translateAlongOwnImpl
  , rotateAroundOwnImpl
  , mirrorAcrossOwnImpl
  , scaleAboutOwnImpl
  , scaleAlongOwnImpl
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis2d, Direction2d, Point2d, Transform2d, Vector2d)
import OpenSolid.Transform (Affine, Orthonormal, Rigid, Uniform)

translateByImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> Vector2d (space @ units) -> a -> b
translateByOwnImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> (a -> Vector2d (space @ units)) -> a -> b
translateInImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> Direction2d space -> Qty units -> a -> b
translateInOwnImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> (a -> Direction2d space) -> Qty units -> a -> b
translateAlongImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> Axis2d (space @ units) -> Qty units -> a -> b
translateAlongOwnImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> (a -> Axis2d (space @ units)) -> Qty units -> a -> b
rotateAroundImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> Point2d (space @ units) -> Angle -> a -> b
rotateAroundOwnImpl ::
  (Transform2d Rigid (space @ units) -> a -> b) -> (a -> Point2d (space @ units)) -> Angle -> a -> b
mirrorAcrossImpl ::
  (Transform2d Orthonormal (space @ units) -> a -> b) -> Axis2d (space @ units) -> a -> b
mirrorAcrossOwnImpl ::
  (Transform2d Orthonormal (space @ units) -> a -> b) -> (a -> Axis2d (space @ units)) -> a -> b
scaleAboutImpl ::
  (Transform2d Uniform (space @ units) -> a -> b) -> Point2d (space @ units) -> Float -> a -> b
scaleAboutOwnImpl ::
  (Transform2d Uniform (space @ units) -> a -> b) -> (a -> Point2d (space @ units)) -> Float -> a -> b
scaleAlongImpl ::
  (Transform2d Affine (space @ units) -> a -> b) -> Axis2d (space @ units) -> Float -> a -> b
scaleAlongOwnImpl ::
  (Transform2d Affine (space @ units) -> a -> b) -> (a -> Axis2d (space @ units)) -> Float -> a -> b
