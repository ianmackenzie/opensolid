module Solve2d
  ( Subdomain
  , init
  , overlaps
  , contains
  , bounds
  , interior
  , enqueueChildren
  )
where

import Bounds2d qualified
import OpenSolid
import Queue (Queue)
import Queue qualified
import Solve1d qualified
import Uv qualified

data Subdomain = Subdomain Solve1d.Subdomain Solve1d.Subdomain

domain :: Subdomain
domain = Subdomain Solve1d.domain Solve1d.domain

init :: Queue Subdomain
init = Queue.singleton domain

overlaps :: Subdomain -> Subdomain -> Bool
overlaps (Subdomain x2 y2) (Subdomain x1 y1) = Solve1d.overlaps x2 x1 && Solve1d.overlaps y2 y1

contains :: Subdomain -> Subdomain -> Bool
contains (Subdomain x2 y2) (Subdomain x1 y1) = Solve1d.contains x2 x1 && Solve1d.contains y2 y1

bounds :: Subdomain -> Uv.Bounds
bounds (Subdomain x y) = Bounds2d.xy (Solve1d.bounds x) (Solve1d.bounds y)

interior :: Subdomain -> Uv.Bounds
interior (Subdomain x y) = Bounds2d.xy (Solve1d.interior x) (Solve1d.interior y)

isAtomic :: Subdomain -> Bool
isAtomic (Subdomain x y) = Solve1d.isAtomic x && Solve1d.isAtomic y

data IsAtomic = IsAtomic deriving (Eq, Show, Error)

enqueueChildren :: Subdomain -> Queue Subdomain -> Result IsAtomic (Queue Subdomain)
enqueueChildren subdomain queue
  | isAtomic subdomain = Error IsAtomic
  | otherwise = do
      let (Subdomain x y) = subdomain
      let (x1, x2) = Solve1d.bisect x
      let (y1, y2) = Solve1d.bisect y
      let xMid = Solve1d.half x
      let yMid = Solve1d.half y
      let updatedQueue =
            queue
              |> Queue.push (Subdomain xMid yMid)
              |> Queue.push (Subdomain xMid y1)
              |> Queue.push (Subdomain xMid y2)
              |> Queue.push (Subdomain x1 yMid)
              |> Queue.push (Subdomain x2 yMid)
              |> Queue.push (Subdomain x1 y1)
              |> Queue.push (Subdomain x1 y2)
              |> Queue.push (Subdomain x2 y1)
              |> Queue.push (Subdomain x2 y2)
      Ok updatedQueue
