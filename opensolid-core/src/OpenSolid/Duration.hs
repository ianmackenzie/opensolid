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
import OpenSolid.Quantity qualified as Quantity

type Duration = Quantity Seconds

zero :: Duration
zero = Quantity.zero

second :: Duration
second = seconds 1.0

seconds :: Number -> Duration
seconds = Quantity.coerce

inSeconds :: Duration -> Number
inSeconds = Quantity.coerce

microsecond :: Duration
microsecond = seconds 1e-6

microseconds :: Number -> Duration
microseconds = (.*. microsecond)

inMicroseconds :: Duration -> Number
inMicroseconds = (./. microsecond)

millisecond :: Duration
millisecond = seconds 1e-3

milliseconds :: Number -> Duration
milliseconds = (.*. millisecond)

inMilliseconds :: Duration -> Number
inMilliseconds = (./. millisecond)

minute :: Duration
minute = seconds 60.0

minutes :: Number -> Duration
minutes = (.*. minute)

inMinutes :: Duration -> Number
inMinutes = (./. minute)

hour :: Duration
hour = minutes 60.0

hours :: Number -> Duration
hours = (.*. hour)

inHours :: Duration -> Number
inHours = (./. hour)
