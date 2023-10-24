module Codegen
  ( Codegen
  , execute
  , generate
  , fail
  , reify
  , reifyType
  , reifyInstances
  , newName
  , map
  , sequence
  , collect
  , nameModule
  , nameBase
  )
where

import DoNotation
import Language.Haskell.TH qualified as TH
import List qualified
import Maybe qualified
import OpenSolid
import Text qualified
import Prelude qualified

newtype Codegen a = Codegen (TH.Q a)

instance (a ~ a') => Bind (Codegen a) a' (Codegen b) where
  bind f (Codegen q) = Codegen (q Prelude.>>= (f >> execute))

instance Fail (Codegen a) where
  fail message = Codegen (Prelude.fail (Text.toChars message))

execute :: Codegen a -> TH.Q a
execute (Codegen q) = q

generate :: a -> Codegen a
generate value = Codegen (Prelude.return value)

reify :: TH.Name -> Codegen TH.Info
reify name = Codegen (TH.reify name)

reifyType :: TH.Name -> Codegen TH.Type
reifyType name = Codegen (TH.reifyType name)

reifyInstances :: TH.Name -> List TH.Type -> Codegen (List TH.InstanceDec)
reifyInstances name types = Codegen (TH.reifyInstances name types)

newName :: Text -> Codegen TH.Name
newName prefix = Codegen (TH.newName (Text.toChars prefix))

map :: (a -> b) -> Codegen a -> Codegen b
map function codegen = do
  value <- codegen
  generate (function value)

sequence :: List (Codegen a) -> Codegen (List a)
sequence [] = generate []
sequence (first : rest) = do
  firstValue <- first
  restValues <- sequence rest
  generate (firstValue : restValues)

collect :: (a -> Codegen b) -> List a -> Codegen (List b)
collect function values = sequence (List.map function values)

nameBase :: TH.Name -> Text
nameBase name = Text.fromChars (TH.nameBase name)

nameModule :: TH.Name -> Maybe Text
nameModule name = Maybe.map Text.fromChars (TH.nameModule name)
