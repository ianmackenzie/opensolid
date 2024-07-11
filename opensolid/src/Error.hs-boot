module Error (Message) where

import Basics
import Data.Kind (Constraint)

type Message :: Type -> Constraint
class Message error

instance Message (List Char)
