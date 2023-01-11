module Direction2d
  ( Direction2d
  , unsafe
  )
where

import OpenSolid

type role Direction2d phantom

data Direction2d coordinates

unsafe :: Float -> Float -> Direction2d coordinates
