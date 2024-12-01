module OpenSolid.API.Class
  ( Class (..)
  , unitless
  , withUnits
  , nestedUnitless
  , nestedWithUnits
  , withStaticFunctions
  , withMemberFunctions
  , withNestedClasses
  )
where

import OpenSolid
import OpenSolid.API.MemberFunction (MemberFunction)
import OpenSolid.API.Name (Name)
import OpenSolid.API.StaticFunction (StaticFunction)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI

data Class where
  Class ::
    FFI value =>
    { id :: FFI.Id
    , staticFunctions :: List (Name, List StaticFunction)
    , memberFunctions :: List (Name, List (MemberFunction value))
    , nestedClasses :: List Class
    } ->
    Class

unitless :: Text -> Class
unitless name =
  Class
    { id = FFI.classId name Nothing
    , staticFunctions = []
    , memberFunctions = [] :: List (Name, List (MemberFunction Int))
    , nestedClasses = []
    }

withUnits :: Text -> Text -> Class
withUnits name units =
  Class
    { id = FFI.classId name (Just units)
    , staticFunctions = []
    , memberFunctions = [] :: List (Name, List (MemberFunction Int))
    , nestedClasses = []
    }

nestedUnitless :: Text -> Text -> Class
nestedUnitless parentName childName =
  Class
    { id = FFI.nestedClassId parentName childName Nothing
    , staticFunctions = []
    , memberFunctions = [] :: List (Name, List (MemberFunction Int))
    , nestedClasses = []
    }

nestedWithUnits :: Text -> Text -> Text -> Class
nestedWithUnits parentName childName units =
  Class
    { id = FFI.nestedClassId parentName childName (Just units)
    , staticFunctions = []
    , memberFunctions = [] :: List (Name, List (MemberFunction Int))
    , nestedClasses = []
    }

withStaticFunctions :: List (Name, List StaticFunction) -> Class -> Class
withStaticFunctions staticFunctions class_ = class_{staticFunctions = staticFunctions}

withMemberFunctions :: FFI value => List (Name, List (MemberFunction value)) -> Class -> Class
withMemberFunctions memberFunctions class_ = class_{memberFunctions = memberFunctions}

withNestedClasses :: List Class -> Class -> Class
withNestedClasses nestedClasses class_ = class_{nestedClasses = nestedClasses}
