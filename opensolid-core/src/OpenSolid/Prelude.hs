module OpenSolid.Prelude
  ( Int
  , Double
  , Bool (True, False)
  , Maybe (Just, Nothing)
  , Either (Left, Right)
  , Ordering (EQ, GT, LT)
  , Char
  , IO
  , Type
  , type (~)
  , Exception
  , Eq ((==), (/=))
  , Ord ((<), (<=), (>=), (>), compare)
  , Show
  , Functor
  , (<$>)
  , Applicative ((<*>))
  , Monad ((>>=), (>>))
  , MonadFail
  , Semigroup ((<>))
  , Foldable
  , Traversable
  , FoldableWithIndex
  , TraversableWithIndex
  , (&&)
  , (||)
  , (+)
  , (-)
  , (/)
  , (*)
  , div
  , mod
  , not
  , otherwise
  , id
  , const
  , ($)
  , (.)
  , fromIntegral
  , fromRational
  , assert
  , abort
  , throw
  , Text
  , List
  , Negation (negative)
  , Addition ((.+.))
  , (+.)
  , (.+)
  , Subtraction ((.-.))
  , (-.)
  , (.-)
  , Multiplication ((.*.))
  , (*.)
  , (.*)
  , Division ((./.))
  , (/.)
  , (./)
  , DotMultiplication (dot)
  , CrossMultiplication (cross)
  , Multiplication# ((#*#))
  , (*#)
  , (#*)
  , Division# ((#/#))
  , (/#)
  , (#/)
  , DotMultiplication# (dot#)
  , CrossMultiplication# (cross#)
  , Composition (compose)
  , CoordinateSystem
  , Defines
  , LocalSpace
  , UvCoordinates
  , UvSpace
  , type (@)
  , Number
  , Intersects (intersects)
  , NonEmpty ((:|))
  , Quantity (Quantity)
  , (.//.)
  , (.%.)
  , pattern NonEmpty
  , (:::) (Named)
  , Result (Success, Failure)
  , Sign (Positive, Negative)
  , Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  , Meters
  , Radians
  , Seconds
  , Unitless
  , type (#*#)
  , type (#/#)
  , (|>)
  , pattern TODO
  )
where

import Control.Exception (Exception, assert, throw)
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified
import Data.Traversable.WithIndex (TraversableWithIndex)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OpenSolid.Arithmetic
  ( Addition ((.+.))
  , CrossMultiplication (cross)
  , CrossMultiplication# (cross#)
  , Division ((./.))
  , Division# ((#/#))
  , DotMultiplication (dot)
  , DotMultiplication# (dot#)
  , Multiplication ((.*.))
  , Multiplication# ((#*#))
  , Negation (negative)
  , Subtraction ((.-.))
  , (#*)
  , (#/)
  , (*#)
  , (*.)
  , (+.)
  , (-.)
  , (.*)
  , (.+)
  , (.-)
  , (./)
  , (/#)
  , (/.)
  )
import OpenSolid.Composition (Composition (compose))
import OpenSolid.CoordinateSystem
  ( CoordinateSystem
  , Defines
  , LocalSpace
  , UvCoordinates
  , UvSpace
  , type (@)
  )
import OpenSolid.Intersects (Intersects (intersects))
import OpenSolid.List (List)
import OpenSolid.Named ((:::) (Named))
import OpenSolid.NonEmpty (NonEmpty ((:|)), pattern NonEmpty)
import OpenSolid.Number (Number)
import OpenSolid.Quantity (Quantity (Quantity), (.%.), (.//.))
import OpenSolid.Result (Result (Failure, Success))
import OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.Tolerance (ApproximateEquality ((~=)), Tolerance, (!=))
import OpenSolid.Units (Meters, Radians, Seconds, Unitless, type (#*#), type (#/#))
import Prelude

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
value |> function = function value

infixl 0 |>

abort :: Text -> a
abort message = error (Data.Text.unpack message)

pattern TODO :: HasCallStack => a
pattern TODO <- (withFrozenCallStack todo -> ())
  where
    TODO = withFrozenCallStack todo

todo :: HasCallStack => a
todo = withFrozenCallStack $ abort "Not implemented"

{-# COMPLETE TODO #-}
