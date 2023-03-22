module Direction2d
  ( Direction2d
  , unsafe
  )
where

import OpenSolid

type role Direction2d nominal

data Direction2d (space :: Type)

unsafe :: Float -> Float -> Direction2d space
