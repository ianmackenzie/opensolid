module OpenSolid.UvEdge
  ( UvEdge (..)
  , left
  , right
  , bottom
  , top
  )
where

data UvEdge
  = Left
  | Right
  | Bottom
  | Top

left :: UvEdge
left = Left

right :: UvEdge
right = Right

bottom :: UvEdge
bottom = Bottom

top :: UvEdge
top = Top
