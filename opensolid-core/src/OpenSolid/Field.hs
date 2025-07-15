module OpenSolid.Field ((:::) (Field), fromLabel) where

import GHC.TypeLits (Symbol)
import OpenSolid.Bootstrap
import OpenSolid.Composition

type (:::) :: Symbol -> Type -> Type
newtype name ::: a = Field a

infix 0 :::

fromLabel :: forall name a. a -> name ::: a
fromLabel = Field

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2)
    (name1 ::: value1, name2 ::: value2)
  where
  field1 >> field2 = (field1, field2)

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2, name3 ::: value3)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3)
  where
  field1 >> (field2, field3) = (field1, field2, field3)

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2, name3 ::: value3, name4 ::: value4)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
  where
  field1 >> (field2, field3, field4) = (field1, field2, field3, field4)

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
  where
  field1 >> (field2, field3, field4, field5) = (field1, field2, field3, field4, field5)

instance
  Composition
    (name1 ::: value1)
    ( name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    )
  where
  field1 >> (field2, field3, field4, field5, field6) =
    (field1, field2, field3, field4, field5, field6)

instance
  Composition
    (name1 ::: value1)
    ( name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    )
  where
  field1 >> (field2, field3, field4, field5, field6, field7) =
    (field1, field2, field3, field4, field5, field6, field7)

instance
  Composition
    (name1 ::: value1)
    ( name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    , name6 ::: value6
    , name7 ::: value7
    , name8 ::: value8
    )
  where
  field1 >> (field2, field3, field4, field5, field6, field7, field8) =
    (field1, field2, field3, field4, field5, field6, field7, field8)

instance HasField name1 (name1 ::: value1, name2 ::: value2) value1 where
  getField (Field value1, _) = value1

instance HasField name2 (name1 ::: value1, name2 ::: value2) value2 where
  getField (_, Field value2) = value2

instance HasField name1 (name1 ::: value1, name2 ::: value2, name3 ::: value3) value1 where
  getField (Field value1, _, _) = value1

instance HasField name2 (name1 ::: value1, name2 ::: value2, name3 ::: value3) value2 where
  getField (_, Field value2, _) = value2

instance HasField name3 (name1 ::: value1, name2 ::: value2, name3 ::: value3) value3 where
  getField (_, _, Field value3) = value3

instance
  HasField
    name1
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    )
    value1
  where
  getField (Field value1, _, _, _) = value1

instance
  HasField
    name2
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    )
    value2
  where
  getField (_, Field value2, _, _) = value2

instance
  HasField
    name3
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    )
    value3
  where
  getField (_, _, Field value3, _) = value3

instance
  HasField
    name4
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    )
    value4
  where
  getField (_, _, _, Field value4) = value4

instance
  HasField
    name1
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    )
    value1
  where
  getField (Field value1, _, _, _, _) = value1

instance
  HasField
    name2
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    )
    value2
  where
  getField (_, Field value2, _, _, _) = value2

instance
  HasField
    name3
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    )
    value3
  where
  getField (_, _, Field value3, _, _) = value3

instance
  HasField
    name4
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    )
    value4
  where
  getField (_, _, _, Field value4, _) = value4

instance
  HasField
    name5
    ( name1 ::: value1
    , name2 ::: value2
    , name3 ::: value3
    , name4 ::: value4
    , name5 ::: value5
    )
    value5
  where
  getField (_, _, _, _, Field value5) = value5

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
  getField (Field value1, _, _, _, _, _) = value1

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
  getField (_, Field value2, _, _, _, _) = value2

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
  getField (_, _, Field value3, _, _, _) = value3

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
  getField (_, _, _, Field value4, _, _) = value4

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
  getField (_, _, _, _, Field value5, _) = value5

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
  getField (_, _, _, _, _, Field value6) = value6

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
  getField (Field value1, _, _, _, _, _, _) = value1

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
  getField (_, Field value2, _, _, _, _, _) = value2

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
  getField (_, _, Field value3, _, _, _, _) = value3

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
  getField (_, _, _, Field value4, _, _, _) = value4

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
  getField (_, _, _, _, Field value5, _, _) = value5

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
  getField (_, _, _, _, _, Field value6, _) = value6

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
  getField (_, _, _, _, _, _, Field value7) = value7

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
  getField (Field value1, _, _, _, _, _, _, _) = value1

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
  getField (_, Field value2, _, _, _, _, _, _) = value2

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
  getField (_, _, Field value3, _, _, _, _, _) = value3

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
  getField (_, _, _, Field value4, _, _, _, _) = value4

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
  getField (_, _, _, _, Field value5, _, _, _) = value5

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
  getField (_, _, _, _, _, Field value6, _, _) = value6

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
  getField (_, _, _, _, _, _, Field value7, _) = value7

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
  getField (_, _, _, _, _, _, _, Field value8) = value8
