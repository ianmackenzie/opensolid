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
  , IOError
  , Maybe (Just, Nothing)
  , Type
  , identity
  , always
  , fromIntegral
  , internalError
  , notImplemented
  , (|>)
  , (<|)
  , type (~)
  , fmap
  , (<*>)
  , (>>=)
  , join
  , pure
  , return
  , fail
  )
where

import Control.Monad (join)
import Data.Kind (Type)
import Data.Type.Equality (type (~))
import Prelude
  ( Bool (False, True)
  , Char
  , Eq ((/=), (==))
  , IO
  , IOError
  , Int
  , Maybe (Just, Nothing)
  , Monad (return, (>>=))
  , MonadFail (fail)
  , Ord (compare, (<), (<=), (>), (>=))
  , Ordering (EQ, GT, LT)
  , Show (show)
  , String
  , fmap
  , fromIntegral
  , not
  , otherwise
  , pure
  , (&&)
  , (<*>)
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

internalError :: String -> a
internalError message = Prelude.error ("Internal error: " Prelude.++ message)

notImplemented :: a
notImplemented = Prelude.error "Not implemented"

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) value function = function value

infixl 0 |>

{-# INLINE (<|) #-}
(<|) :: (a -> b) -> a -> b
(<|) = (Prelude.$)

infixr 0 <|

{-# INLINE identity #-}
identity :: a -> a
identity value = value

{-# INLINE always #-}
always :: a -> b -> a
always value _ = value
