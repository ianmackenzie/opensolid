module OpenSolid.Show (constructor, constructor2, constructor3) where

import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified
import Text.Show qualified
import Prelude

constructor :: Show a => Int -> Text -> a -> ShowS
constructor precedence name value =
  constructorN precedence name [field value]

constructor2 :: (Show a, Show b) => Int -> Text -> a -> b -> ShowS
constructor2 precedence name value1 value2 =
  constructorN precedence name [field value1, field value2]

constructor3 :: (Show a, Show b, Show c) => Int -> Text -> a -> b -> c -> ShowS
constructor3 precedence name value1 value2 value3 =
  constructorN precedence name [field value1, field value2, field value3]

constructorN :: Int -> Text -> [ShowS] -> ShowS
constructorN precedence name fields =
  Text.Show.showParen (precedence > 10) $
    literal name . foldr (.) id (fmap (space .) fields)

literal :: Text -> ShowS
literal = Text.Show.showString . Data.Text.unpack

space :: ShowS
space = Text.Show.showString " "

field :: Show a => a -> ShowS
field = Text.Show.showsPrec 11
