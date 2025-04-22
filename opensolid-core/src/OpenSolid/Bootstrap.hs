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
  , always
  , (&)
  , fromIntegral
  , internalError
  , exception
  , pattern TODO
  , (|>)
  , ($)
  , type (~)
  , HasCallStack
  , withFrozenCallStack
  )
where

import Control.Concurrent.Async (Async)
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified
import Data.Traversable.WithIndex (TraversableWithIndex)
import Data.Type.Equality (type (~))
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
(|>) value function = function value

infixl 0 |>

{-# INLINE identity #-}
identity :: a -> a
identity value = value

{-# INLINE always #-}
always :: a -> b -> a
always value _ = value

{-# INLINE (&) #-}
(&) :: (a -> b) -> a -> b
(&) f a = f a

infixl 0 &
