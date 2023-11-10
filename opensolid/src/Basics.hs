module Basics
  ( List
  , String
  , ifThenElse
  , fromString
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
  , ($)
  , (&&)
  , (||)
  , IO
  , IOError
  , Maybe (Just, Nothing)
  , Type
  , HasField (getField)
  , id
  , const
  , fromIntegral
  , flip
  , internalError
  , notImplemented
  , (|>)
  , type (~)
  , Functor (fmap)
  , (<$>)
  , Applicative (pure, (<*>))
  , Monad ((>>=), (>>), return)
  , MonadFail (fail)
  , (.)
  )
where

import Data.Kind (Type)
import Data.Type.Equality (type (~))
import GHC.Records (HasField (getField))
import Prelude
  ( Applicative (pure, (<*>))
  , Bool (False, True)
  , Char
  , Eq ((/=), (==))
  , Functor (fmap)
  , IO
  , IOError
  , Int
  , Maybe (Just, Nothing)
  , Monad (return, (>>), (>>=))
  , MonadFail (fail)
  , Ord (compare, (<), (<=), (>), (>=))
  , Ordering (EQ, GT, LT)
  , Show (show)
  , String
  , const
  , flip
  , fromIntegral
  , id
  , not
  , otherwise
  , ($)
  , (&&)
  , (.)
  , (<$>)
  , (||)
  )
import Prelude qualified

type List a = [a]

{-# INLINE ifThenElse #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch _ = ifBranch
ifThenElse False _ elseBranch = elseBranch

{-# INLINE fromString #-}
fromString :: String -> String
fromString = id

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

internalError :: String -> a
internalError = Prelude.error

notImplemented :: a
notImplemented = internalError "Not implemented"

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) value function = function value

infixl 0 |>
