module Units (Unitless, Product, Quotient, Squared) where

import Basics

data Unitless

type Prod :: Type -> Type -> Type
type family Prod units1 units2

type Quot :: Type -> Type -> Type
type family Quot units1 units2

type Sqr :: Type -> Type
type family Sqr units = squaredUnits | squaredUnits -> units

type Product units1 units2 units3 =
  ( Prod units1 units2 ~ units3
  , Prod units2 units1 ~ units3
  , Quot units3 units1 ~ units2
  , Quot units3 units2 ~ units1
  )

type Quotient units1 units2 units3 =
  ( Quot units1 units2 ~ units3
  , Prod units2 units3 ~ units1
  , Prod units3 units2 ~ units1
  , Quot units1 units3 ~ units2
  )

type Squared units1 units2 =
  ( Sqr units1 ~ units2
  , Prod units1 units1 ~ units2
  , Quot units2 units1 ~ units1
  )
