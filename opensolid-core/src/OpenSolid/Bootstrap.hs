-- Needed for List and NonEmpty HasField instances
-- (should hopefully be safe having those instances here,
-- since pretty much *any* modules that use OpenSolid
-- will indirectly import this module, even if e.g.
-- they don't import OpenSolid.List or OpenSolid.NonEmpty)
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.Bootstrap
  ( List
  , Text
  , ifThenElse
  , fromString
  , fromInteger
  , otherwise
  , Int
  , Eq ((==), (/=))
  , Ord ((<), (<=), (>=), (>), compare)
  , Ordering (EQ, GT, LT)
  , Show
  , Foldable
  , FoldableWithIndex
  , Traversable
  , TraversableWithIndex
  , Bool (True, False)
  , Char
  , not
  , (&&)
  , (||)
  , (<>)
  , IO
  , Async
  , Maybe (Just, Nothing)
  , Type
  , identity
  , return
  , always
  , (#)
  , (##)
  , (###)
  , fromIntegral
  , internalError
  , exception
  , pattern TODO
  , (|>)
  , ($)
  , type (~)
  , HasCallStack
  , withFrozenCallStack
  , HasField (getField)
  )
where

import Control.Concurrent.Async (Async)
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Kind (Type)
import Data.List qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified
import Data.Text (Text)
import Data.Text qualified
import Data.Traversable.WithIndex (TraversableWithIndex)
import Data.Type.Equality (type (~))
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Prelude
  ( Bool (False, True)
  , Char
  , Eq ((/=), (==))
  , Foldable
  , IO
  , Int
  , Maybe (Just, Nothing)
  , Ord (compare, (<), (<=), (>), (>=))
  , Ordering (EQ, GT, LT)
  , Show
  , Traversable
  , fromIntegral
  , not
  , otherwise
  , ($)
  , (&&)
  , (<>)
  , (||)
  )
import Prelude qualified

type List a = [a]

{-# INLINE ifThenElse #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch _ = ifBranch
ifThenElse False _ elseBranch = elseBranch

{-# INLINE fromString #-}
fromString :: List Char -> Text
fromString = Data.Text.pack

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

internalError :: HasCallStack => Text -> a
internalError message = withFrozenCallStack $ abort ("Internal error: " <> message)

exception :: HasCallStack => Text -> a
exception message = withFrozenCallStack $ abort ("Exception occurred: " <> message)

pattern TODO :: HasCallStack => a
pattern TODO <- (withFrozenCallStack todo -> ())
  where
    TODO = withFrozenCallStack todo

{-# COMPLETE TODO #-}

todo :: HasCallStack => a
todo = withFrozenCallStack $ abort "Not implemented"

abort :: HasCallStack => Text -> a
abort message = Prelude.error (Data.Text.unpack message)

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
value |> function = function value

infixl 0 |>

{-# INLINE identity #-}
identity :: a -> a
identity value = value

{-# INLINE return #-}
return :: a -> a
return value = value

{-# INLINE always #-}
always :: a -> b -> a
always value _ = value

{-# INLINE (#) #-}
(#) :: (a -> b) -> a -> b
(#) function value = function value

infixl 1 #

{-# INLINE (##) #-}
(##) :: (a -> b) -> a -> b
(##) function value = function value

infixl 2 ##

{-# INLINE (###) #-}
(###) :: (a -> b) -> a -> b
(###) function value = function value

infixl 3 ###

instance HasField "length" (List a) Int where
  getField = Data.List.length

instance HasField "first" (NonEmpty a) a where
  getField = Data.List.NonEmpty.head

instance HasField "rest" (NonEmpty a) [a] where
  getField = Data.List.NonEmpty.tail

instance HasField "last" (NonEmpty a) a where
  getField = Data.List.NonEmpty.last

instance HasField "length" (NonEmpty a) Int where
  getField = Data.List.NonEmpty.length
