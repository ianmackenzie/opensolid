module OpenSolid.Error (Message) where

import Data.Kind (Constraint, Type)
import OpenSolid.List (List)
import Prelude (Char)

type Message :: Type -> Constraint
class Message error

instance Message (List Char)
