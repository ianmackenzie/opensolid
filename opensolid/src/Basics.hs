module Basics
  ( List
  , String
  , ifThenElse
  , fromInteger
  , otherwise
  , Int
  , Eq ((==), (/=))
  , Ord ((<), (<=), (>=), (>), compare)
  , Ordering (EQ, GT, LT)
  , Show (show)
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
  , notImplemented
  , (|>)
  , (<|)
  , (<<)
  , (>>)
  , type (~)
  , return
  , pure
  )
where

import Control.Concurrent.Async (Async)
import Data.Kind (Type)
import Data.Type.Equality (type (~))
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
  , Show (show)
  , String
  , fromIntegral
  , not
  , otherwise
  , (&&)
  , (||)
  )
import Prelude qualified

type List a = [a]

{-# INLINE ifThenElse #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch _ = ifBranch
ifThenElse False _ elseBranch = elseBranch

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

internalError :: HasCallStack => String -> a
internalError message = withFrozenCallStack (Prelude.error ("Internal error: " Prelude.++ message))

notImplemented :: HasCallStack => a
notImplemented = withFrozenCallStack (Prelude.error "Not implemented")

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) value function = function value

infixl 0 |>

{-# INLINE (<|) #-}
(<|) :: (a -> b) -> a -> b
(<|) = (Prelude.$)

infixr 0 <|

{-# INLINE (>>) #-}
(>>) :: (a -> b) -> (b -> c) -> a -> c
f >> g = g << f

{-# INLINE (<<) #-}
(<<) :: (b -> c) -> (a -> b) -> a -> c
(<<) = (Prelude..)

{-# INLINE identity #-}
identity :: a -> a
identity value = value

{-# INLINE always #-}
always :: a -> b -> a
always value _ = value

{-# INLINE pure #-}
pure :: a -> a
pure = identity

{-# INLINE return #-}
return :: a -> a
return = identity
