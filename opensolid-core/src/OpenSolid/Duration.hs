module OpenSolid.Duration
  ( Duration
  , zero
  , second
  , seconds
  , inSeconds
  , millisecond
  , milliseconds
  , inMilliseconds
  , microsecond
  , microseconds
  , inMicroseconds
  , minute
  , minutes
  , inMinutes
  , hour
  , hours
  , inHours
  )
where

import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Units (Seconds)

type Duration = Qty Seconds

zero :: Duration
zero = Qty.zero

second :: Duration
second = seconds 1.0

seconds :: Float -> Duration
seconds = Qty.coerce

inSeconds :: Duration -> Float
inSeconds = Qty.coerce

microsecond :: Duration
microsecond = seconds 1e-6

microseconds :: Float -> Duration
microseconds = (* microsecond)

inMicroseconds :: Duration -> Float
inMicroseconds = (/ microsecond)

millisecond :: Duration
millisecond = seconds 1e-3

milliseconds :: Float -> Duration
milliseconds = (* millisecond)

inMilliseconds :: Duration -> Float
inMilliseconds = (/ millisecond)

minute :: Duration
minute = seconds 60.0

minutes :: Float -> Duration
minutes = (* minute)

inMinutes :: Duration -> Float
inMinutes = (/ minute)

hour :: Duration
hour = minutes 60.0

hours :: Float -> Duration
hours = (* hour)

inHours :: Duration -> Float
inHours = (/ hour)
