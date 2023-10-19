module OpenSolidAPI.Range (range) where

import Internal (Class, cls, method, static)
import OpenSolid
import Range qualified

range :: Class
range =
  cls ''Range.Range ['Range.minValue, 'Range.maxValue] $
    [ static 'Range.unsafe ["min", "max"]
    , static 'Range.constant ["value"]
    , -- TODO: from doesn't work in Python
      -- , static 'Range.from ["a", "b"]
      static 'Range.hull3 ["a", "b", "c"]
    , method 'Range.minValue ["range"]
    , method 'Range.maxValue ["range"]
    , method 'Range.midpoint ["range"]
    , -- TODO: support tuple
      -- , method 'Range.endpoints ["range"]
      method 'Range.width ["range"]
    , method 'Range.squared ["range"]
    , method 'Range.includes ["value", "range"]
    , method 'Range.approximatelyIncludes ["value", "range"]
    , method 'Range.contains ["range2", "range1"]
    , method 'Range.isContainedIn ["range1", "range2"]
    , method 'Range.tolerant ["range"]
    , -- TODO: support tuples
      -- , method 'Range.bisect ["range"]
      method 'Range.isAtomic ["range"]
    , method 'Range.abs ["range"]
    , method 'Range.sqrt ["range"]
    , static 'Range.hypot2 ["range1", "range2"]
    , static 'Range.hypot3 ["range1", "range2", "range3"]
    , static 'Range.aggregate2 ["range1", "range2"]
    , static 'Range.aggregate3 ["range1", "range2", "range3"]
    , static 'Range.min ["range1", "range2"]
    , static 'Range.max ["range1", "range2"]
    , -- TODO: support NonEmpty
      -- , 'Range.minimum
      -- , 'Range.maximum
      -- , 'Range.smallest
      -- , 'Range.largest
      static 'Range.smaller ["first", "second"]
    , static 'Range.larger ["first", "second"]
    , method 'Range.sin ["range"]
    , method 'Range.cos ["range"]
    , static 'Range.interpolate ["range", "t"]
    , static 'Range.interpolationParameter ["range", "value"]
    , -- TODO: support functions
      -- , 'Range.any
      -- , 'Range.all
      -- , 'Range.find
      -- , 'Range.find2
      -- , 'Range.resolve
      -- , 'Range.resolve2
      -- , 'Range.recurse
      -- , 'Range.recurse2
      method 'Range.resolution ["range"]
    , method 'Range.intersects ["range1", "range2"]
    , method 'Range.intersection ["range1", "range2"]
    -- TODO: support Random.Generator
    -- , 'Range.generator
    ]
