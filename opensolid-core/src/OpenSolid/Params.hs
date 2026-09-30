-- Needed for SetField class:
-- GHC will explicitly specify the otherwise-ambiguous 'name' parameter
-- whenever it generates setField calls
{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.Params
  ( type (:::) ((:::))
  , SetField (setField)
  , with
  )
where

import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownSymbol, Symbol)
import Prelude (type (~))

----- Parameter type -----

newtype (name :: Symbol) ::: value where
  (:::) :: forall name -> value -> name ::: value

infix 0 :::

----- Overloaded label support -----

instance
  (KnownSymbol name1, name1 ~ name2, value1 ~ value2) =>
  IsLabel name1 (value1 -> name2 ::: value2)
  where
  fromLabel = (name1 :::)

----- SetField class -----

class SetField (name :: Symbol) value input output | name value input -> output where
  setField :: value -> input -> output

----- Single parameter field access -----

instance HasField name (name ::: value) value where
  {-# INLINE getField #-}
  getField (_ ::: value) = value

----- Two parameters field access -----

instance
  HasField
    name1
    (name1 ::: value1, name2 ::: value2)
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _) = value

instance
  HasField
    name2
    (name1 ::: value1, name2 ::: value2)
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value) = value

----- Three parameters field access -----

instance
  HasField
    name1
    (name1 ::: value1, name2 ::: value2, name3 ::: value3)
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _) = value

instance
  HasField
    name2
    (name1 ::: value1, name2 ::: value2, name3 ::: value3)
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _) = value

instance
  HasField
    name3
    (name1 ::: value1, name2 ::: value2, name3 ::: value3)
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value) = value

----- Four parameters field access -----

instance
  HasField
    name1
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _, _) = value

instance
  HasField
    name2
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _, _) = value

instance
  HasField
    name3
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value, _) = value

instance
  HasField
    name4
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
    value4
  where
  {-# INLINE getField #-}
  getField (_, _, _, _ ::: value) = value

----- Five parameters field access -----

instance
  HasField
    name1
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _, _, _) = value

instance
  HasField
    name2
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _, _, _) = value

instance
  HasField
    name3
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value, _, _) = value

instance
  HasField
    name4
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
    value4
  where
  {-# INLINE getField #-}
  getField (_, _, _, _ ::: value, _) = value

instance
  HasField
    name5
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
    value5
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _ ::: value) = value

----- Six parameters field access -----

instance
  HasField
    name1
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _, _, _, _) = value

instance
  HasField
    name2
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _, _, _, _) = value

instance
  HasField
    name3
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value, _, _, _) = value

instance
  HasField
    name4
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
    value4
  where
  {-# INLINE getField #-}
  getField (_, _, _, _ ::: value, _, _) = value

instance
  HasField
    name5
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
    value5
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _ ::: value, _) = value

instance
  HasField
    name6
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
    value6
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _ ::: value) = value

----- Seven parameters field access -----

instance
  HasField
    name1
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _, _, _, _, _) = value

instance
  HasField
    name2
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _, _, _, _, _) = value

instance
  HasField
    name3
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value, _, _, _, _) = value

instance
  HasField
    name4
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    value4
  where
  {-# INLINE getField #-}
  getField (_, _, _, _ ::: value, _, _, _) = value

instance
  HasField
    name5
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    value5
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _ ::: value, _, _) = value

instance
  HasField
    name6
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    value6
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _ ::: value, _) = value

instance
  HasField
    name7
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    value7
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _ ::: value) = value

----- Eight parameters field access -----

instance
  HasField
    name1
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _, _, _, _, _, _) = value

instance
  HasField
    name2
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _, _, _, _, _, _) = value

instance
  HasField
    name3
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value, _, _, _, _, _) = value

instance
  HasField
    name4
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value4
  where
  {-# INLINE getField #-}
  getField (_, _, _, _ ::: value, _, _, _, _) = value

instance
  HasField
    name5
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value5
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _ ::: value, _, _, _) = value

instance
  HasField
    name6
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value6
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _ ::: value, _, _) = value

instance
  HasField
    name7
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value7
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _ ::: value, _) = value

instance
  HasField
    name8
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    value8
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _, _ ::: value) = value

----- Nine parameters field access -----

instance
  HasField
    name1
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _, _, _, _, _, _, _) = value

instance
  HasField
    name2
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _, _, _, _, _, _, _) = value

instance
  HasField
    name3
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value, _, _, _, _, _, _) = value

instance
  HasField
    name4
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value4
  where
  {-# INLINE getField #-}
  getField (_, _, _, _ ::: value, _, _, _, _, _) = value

instance
  HasField
    name5
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value5
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _ ::: value, _, _, _, _) = value

instance
  HasField
    name6
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value6
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _ ::: value, _, _, _) = value

instance
  HasField
    name7
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value7
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _ ::: value, _, _) = value

instance
  HasField
    name8
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value8
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _, _ ::: value, _) = value

instance
  HasField
    name9
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    )
    value9
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _, _, _ ::: value) = value

----- Ten parameters field access -----

instance
  HasField
    name1
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value1
  where
  {-# INLINE getField #-}
  getField (_ ::: value, _, _, _, _, _, _, _, _, _) = value

instance
  HasField
    name2
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value2
  where
  {-# INLINE getField #-}
  getField (_, _ ::: value, _, _, _, _, _, _, _, _) = value

instance
  HasField
    name3
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value3
  where
  {-# INLINE getField #-}
  getField (_, _, _ ::: value, _, _, _, _, _, _, _) = value

instance
  HasField
    name4
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value4
  where
  {-# INLINE getField #-}
  getField (_, _, _, _ ::: value, _, _, _, _, _, _) = value

instance
  HasField
    name5
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value5
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _ ::: value, _, _, _, _, _) = value

instance
  HasField
    name6
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value6
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _ ::: value, _, _, _, _) = value

instance
  HasField
    name7
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value7
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _ ::: value, _, _, _) = value

instance
  HasField
    name8
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value8
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _, _ ::: value, _, _) = value

instance
  HasField
    name9
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value9
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _, _, _ ::: value, _) = value

instance
  HasField
    name10
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    , name9 ::: value9
    , name10 ::: value10
    )
    value10
  where
  {-# INLINE getField #-}
  getField (_, _, _, _, _, _, _, _, _, _ ::: value) = value

----- Params construction -----

with :: ()
with = ()

instance SetField name1 value1 () (name1 ::: value1) where
  setField value1 () = (name1 ::: value1)

instance SetField name2 value2 (name1 ::: value1) (name1 ::: value1, name2 ::: value2) where
  setField value2 param1 = (param1, name2 ::: value2)

instance
  SetField
    name3
    value3
    (name1 ::: value1, name2 ::: value2)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3)
  where
  setField value3 (param1, param2) = (param1, param2, name3 ::: value3)

instance
  SetField
    name4
    value4
    (name1 ::: value1, name2 ::: value2, name3 ::: value3)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
  where
  setField value4 (param1, param2, param3) = (param1, param2, param3, name4 ::: value4)

instance
  SetField
    name5
    value5
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
  where
  setField value5 (param1, param2, param3, param4) =
    (param1, param2, param3, param4, name5 ::: value5)

----- Function calls -----

instance
  (name ~ name1, value ~ value1) =>
  SetField name value (name1 ::: value1 -> result) result
  where
  {-# INLINE setField #-}
  setField value function = function (name ::: value)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ((name1 ::: value1, name2 ::: value2) -> result)
    (name2 ::: value2 -> result)
  where
  {-# INLINE setField #-}
  setField value function param2 = function (name ::: value, param2)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ((name1 ::: value1, name2 ::: value2, name3 ::: value3) -> result)
    ((name2 ::: value2, name3 ::: value3) -> result)
  where
  {-# INLINE setField #-}
  setField value function (param2, param3) = function (name ::: value, param2, param3)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ((name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4) -> result)
    ((name2 ::: value2, name3 ::: value3, name4 ::: value4) -> result)
  where
  {-# INLINE setField #-}
  setField value function (param2, param3, param4) =
    function (name ::: value, param2, param3, param4)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ( ( name1 ::: value1
      , name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      ) ->
      result
    )
    ( ( name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      ) ->
      result
    )
  where
  {-# INLINE setField #-}
  setField value function (param2, param3, param4, param5) =
    function (name ::: value, param2, param3, param4, param5)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ( ( name1 ::: value1
      , name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      ) ->
      result
    )
    ( ( name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      ) ->
      result
    )
  where
  {-# INLINE setField #-}
  setField value function (param2, param3, param4, param5, param6) =
    function (name ::: value, param2, param3, param4, param5, param6)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ( ( name1 ::: value1
      , name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      ) ->
      result
    )
    ( ( name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      ) ->
      result
    )
  where
  {-# INLINE setField #-}
  setField value function (param2, param3, param4, param5, param6, param7) =
    function (name ::: value, param2, param3, param4, param5, param6, param7)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ( ( name1 ::: value1
      , name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      , name8 ::: value8
      ) ->
      result
    )
    ( ( name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      , name8 ::: value8
      ) ->
      result
    )
  where
  {-# INLINE setField #-}
  setField value function (param2, param3, param4, param5, param6, param7, param8) =
    function (name ::: value, param2, param3, param4, param5, param6, param7, param8)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ( ( name1 ::: value1
      , name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      , name8 ::: value8
      , name9 ::: value9
      ) ->
      result
    )
    ( ( name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      , name8 ::: value8
      , name9 ::: value9
      ) ->
      result
    )
  where
  {-# INLINE setField #-}
  setField value function (param2, param3, param4, param5, param6, param7, param8, param9) =
    function (name ::: value, param2, param3, param4, param5, param6, param7, param8, param9)

instance
  (name ~ name1, value ~ value1) =>
  SetField
    name
    value
    ( ( name1 ::: value1
      , name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      , name8 ::: value8
      , name9 ::: value9
      , name10 ::: value10
      ) ->
      result
    )
    ( ( name2 ::: value2
      , name3 ::: value3
      , name4 ::: value4
      , name5 ::: value5
      , name6 ::: value6
      , name7 ::: value7
      , name8 ::: value8
      , name9 ::: value9
      , name10 ::: value10
      ) ->
      result
    )
  where
  {-# INLINE setField #-}
  setField value function (param2, param3, param4, param5, param6, param7, param8, param9, param10) =
    function (name ::: value, param2, param3, param4, param5, param6, param7, param8, param9, param10)
