module OpenSolid.Named ((:::) (Named), fromLabel) where

import GHC.TypeLits (Symbol)
import OpenSolid.Bootstrap
import OpenSolid.Composition

type (:::) :: Symbol -> Type -> Type
newtype name ::: a = Named a

infix 0 :::

fromLabel :: forall name a. a -> name ::: a
fromLabel = Named

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2)
    (name1 ::: value1, name2 ::: value2)
  where
  named1 >> named2 = (named1, named2)

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2, name3 ::: value3)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3)
  where
  named1 >> (named2, named3) = (named1, named2, named3)

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2, name3 ::: value3, name4 ::: value4)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4)
  where
  named1 >> (named2, named3, named4) = (named1, named2, named3, named4)

instance
  Composition
    (name1 ::: value1)
    (name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
    (name1 ::: value1, name2 ::: value2, name3 ::: value3, name4 ::: value4, name5 ::: value5)
  where
  named1 >> (named2, named3, named4, named5) = (named1, named2, named3, named4, named5)

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
  named1 >> (named2, named3, named4, named5, named6) =
    (named1, named2, named3, named4, named5, named6)

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
  named1 >> (named2, named3, named4, named5, named6, named7) =
    (named1, named2, named3, named4, named5, named6, named7)

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
  named1 >> (named2, named3, named4, named5, named6, named7, named8) =
    (named1, named2, named3, named4, named5, named6, named7, named8)

instance HasField name1 (name1 ::: value1, name2 ::: value2) value1 where
  getField (Named value1, _) = value1

instance HasField name2 (name1 ::: value1, name2 ::: value2) value2 where
  getField (_, Named value2) = value2

instance HasField name1 (name1 ::: value1, name2 ::: value2, name3 ::: value3) value1 where
  getField (Named value1, _, _) = value1

instance HasField name2 (name1 ::: value1, name2 ::: value2, name3 ::: value3) value2 where
  getField (_, Named value2, _) = value2

instance HasField name3 (name1 ::: value1, name2 ::: value2, name3 ::: value3) value3 where
  getField (_, _, Named value3) = value3

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
  getField (Named value1, _, _, _) = value1

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
  getField (_, Named value2, _, _) = value2

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
  getField (_, _, Named value3, _) = value3

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
  getField (_, _, _, Named value4) = value4

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
  getField (Named value1, _, _, _, _) = value1

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
  getField (_, Named value2, _, _, _) = value2

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
  getField (_, _, Named value3, _, _) = value3

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
  getField (_, _, _, Named value4, _) = value4

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
  getField (_, _, _, _, Named value5) = value5

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
  getField (Named value1, _, _, _, _, _) = value1

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
  getField (_, Named value2, _, _, _, _) = value2

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
  getField (_, _, Named value3, _, _, _) = value3

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
  getField (_, _, _, Named value4, _, _) = value4

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
  getField (_, _, _, _, Named value5, _) = value5

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
  getField (_, _, _, _, _, Named value6) = value6

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
  getField (Named value1, _, _, _, _, _, _) = value1

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
  getField (_, Named value2, _, _, _, _, _) = value2

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
  getField (_, _, Named value3, _, _, _, _) = value3

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
  getField (_, _, _, Named value4, _, _, _) = value4

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
  getField (_, _, _, _, Named value5, _, _) = value5

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
  getField (_, _, _, _, _, Named value6, _) = value6

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
  getField (_, _, _, _, _, _, Named value7) = value7

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
  getField (Named value1, _, _, _, _, _, _, _) = value1

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
  getField (_, Named value2, _, _, _, _, _, _) = value2

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
  getField (_, _, Named value3, _, _, _, _, _) = value3

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
  getField (_, _, _, Named value4, _, _, _, _) = value4

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
  getField (_, _, _, _, Named value5, _, _, _) = value5

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
  getField (_, _, _, _, _, Named value6, _, _) = value6

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
  getField (_, _, _, _, _, _, Named value7, _) = value7

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
  getField (_, _, _, _, _, _, _, Named value8) = value8
