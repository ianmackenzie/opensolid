module Maybe (orError) where

import Basics
import Error (Error)
import {-# SOURCE #-} Result (Result)

orError :: Error x => x -> Maybe a -> Result x a
