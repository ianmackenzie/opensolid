module Show (primitive) where

import qualified List
import OpenSolid
import qualified String
import qualified Prelude

primitive :: Int -> String -> List Float -> Prelude.ShowS
primitive precedence constructorName arguments =
    let argumentStrings = List.map String.fromFloat arguments
        expressionString = String.join " " (constructorName : argumentStrings)
     in Prelude.showParen (precedence > 10) (Prelude.showString (String.toList expressionString))
