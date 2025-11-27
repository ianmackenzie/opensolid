{-| Re-exported functions and types from the base Prelude,
a few other common Haskell modules, and several OpenSolid modules.
-}
module OpenSolid.Prelude
  ( Int -- base Prelude
  , Double
  , Bool (True, False)
  , Maybe (Just, Nothing)
  , Either (Left, Right)
  , Ordering (EQ, GT, LT)
  , Char
  , IO
  , type (~)
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
  , (&&)
  , (||)
  , (+)
  , (-)
  , (/)
  , (*)
  , min
  , max
  , floor
  , ceiling
  , round
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
  , (&) -- Data.Function
  , Type -- Data.Kind
  , Exception -- Control.Exception
  , assert
  , throw
  , HasCallStack -- GHC.Stack
  , Text -- Data.Text
  , NonEmpty ((:|)) -- Data.List.NonEmpty
  , List -- OpenSolid
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
  , Multiplication_ ((?*?))
  , (*?)
  , (?*)
  , Division_ ((?/?))
  , (/?)
  , (?/)
  , DotMultiplication_ (dot_)
  , CrossMultiplication_ (cross_)
  , Composition (compose)
  , UvSpace
  , Number
  , Intersects (intersects)
  , Quantity (Quantity)
  , (.//.)
  , (.%.)
  , pattern NonEmpty
  , (:::) (Named)
  , Result (Ok, Error)
  , Sign (Positive, Negative)
  , Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  , Meters
  , Radians
  , Seconds
  , Unitless
  , type (?*?)
  , type (?/?)
  , pattern TODO
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Function
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import OpenSolid.Arithmetic
  ( Addition ((.+.))
  , CrossMultiplication (cross)
  , CrossMultiplication_ (cross_)
  , Division ((./.))
  , Division_ ((?/?))
  , DotMultiplication (dot)
  , DotMultiplication_ (dot_)
  , Multiplication ((.*.))
  , Multiplication_ ((?*?))
  , Negation (negative)
  , Subtraction ((.-.))
  , (*.)
  , (*?)
  , (+.)
  , (-.)
  , (.*)
  , (.+)
  , (.-)
  , (./)
  , (/.)
  , (/?)
  , (?*)
  , (?/)
  )
import OpenSolid.Composition (Composition (compose))
import OpenSolid.CoordinateSystem (UvSpace)
import OpenSolid.Intersects (Intersects (intersects))
import OpenSolid.List (List)
import OpenSolid.Named ((:::) (Named))
import OpenSolid.NonEmpty (pattern NonEmpty)
import OpenSolid.Number (Number)
import OpenSolid.Quantity (Quantity (Quantity), (.%.), (.//.))
import OpenSolid.Result (Result (Error, Ok))
import OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.Todo (pattern TODO)
import OpenSolid.Tolerance (ApproximateEquality ((~=)), Tolerance, (!=))
import OpenSolid.Units (Meters, Radians, Seconds, Unitless, type (?*?), type (?/?))
import Prelude
