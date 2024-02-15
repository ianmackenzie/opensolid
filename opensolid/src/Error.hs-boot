module Error (Error) where

import Basics (String)
import Data.Kind (Constraint, Type)

type Error :: Type -> Constraint
class Error error

instance Error String
