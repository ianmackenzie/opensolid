module OpenSolid.API.Class
  ( Class (..)
  , abstract
  , concrete
  )
where

import OpenSolid
import OpenSolid.API.MemberFunction (MemberFunction)
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.StaticFunction (StaticFunction)
import OpenSolid.FFI (FFI)

data Class where
  Class ::
    FFI value =>
    { name :: Name
    , units :: Maybe Name
    , staticFunctions :: List (Name, List StaticFunction)
    , memberFunctions :: List (Name, List (MemberFunction value))
    } ->
    Class

abstract :: Text -> List (Name, List StaticFunction) -> Class
abstract name staticFunctions =
  Class
    { name = Name.parse name
    , units = Nothing
    , staticFunctions
    , memberFunctions = [] :: List (Name, List (MemberFunction Int))
    }

concrete :: FFI value => Text -> Text -> List (Name, List (MemberFunction value)) -> Class
concrete name units memberFunctions =
  Class
    { name = Name.parse name
    , units = Just (Name.parse units)
    , staticFunctions = []
    , memberFunctions
    }
