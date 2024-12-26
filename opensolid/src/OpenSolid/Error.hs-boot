module OpenSolid.Error (Message) where

import Data.Kind (Constraint)
import OpenSolid.Bootstrap

type Message :: Type -> Constraint
class Message error

instance Message (List Char)
