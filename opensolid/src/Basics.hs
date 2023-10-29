module Basics
  ( List
  , String
  , ifThenElse
  , fromString
  , fromInteger
  , otherwise
  , Int
  , Eq ((==), (/=))
  , Ord ((<), (<=), (>=), (>))
  , Show
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
  , identity
  , always
  , flip
  , internalError
  , notImplemented
  , (|>)
  , type (~)
  , Monad ((>>=), (>>), return)
  , MonadFail (fail)
  , (.)
  )
where

import Data.Kind (Type)
import Data.Type.Equality (type (~))
import GHC.Records (HasField (getField))
import Prelude
  ( Bool (False, True)
  , Char
  , Eq ((/=), (==))
  , IO
  , IOError
  , Int
  , Maybe (Just, Nothing)
  , Monad (return, (>>), (>>=))
  , MonadFail (fail)
  , Ord ((<), (<=), (>), (>=))
  , Show
  , String
  , flip
  , not
  , otherwise
  , ($)
  , (&&)
  , (.)
  , (||)
  )
import Prelude qualified

type List a = [a]

ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch _ = ifBranch
ifThenElse False _ elseBranch = elseBranch

fromString :: String -> String
fromString = identity

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

identity :: a -> a
identity = Prelude.id

always :: a -> b -> a
always = Prelude.const

internalError :: String -> a
internalError = Prelude.error

notImplemented :: a
notImplemented = internalError "Not implemented"

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) value function = function value

infixl 0 |>
