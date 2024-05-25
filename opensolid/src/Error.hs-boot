module Error (Error) where

import Basics
import Data.Kind (Constraint)

type Error :: Type -> Constraint
class Error error

instance Error (List Char)
