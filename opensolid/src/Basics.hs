module Basics
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
  , Bool (True, False)
  , Char
  , not
  , (&&)
  , (||)
  , IO
  , Async
  , Maybe (Just, Nothing)
  , Type
  , identity
  , always
  , fromIntegral
  , internalError
  , pattern TODO
  , (|>)
  , ($)
  , type (~)
  , HasCallStack
  , withFrozenCallStack
  , Typeable
  , Known
  )
where

import Control.Concurrent.Async (Async)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified
import Data.Type.Equality (type (~))
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Prelude
  ( Bool (False, True)
  , Char
  , Eq ((/=), (==))
  , IO
  , Int
  , Maybe (Just, Nothing)
  , Ord (compare, (<), (<=), (>), (>=))
  , Ordering (EQ, GT, LT)
  , Show
  , fromIntegral
  , not
  , otherwise
  , ($)
  , (&&)
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
internalError message = withFrozenCallStack $ abort (Prelude.mappend "Internal error: " message)

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

type Known a = (Eq a, Show a, Typeable a)
