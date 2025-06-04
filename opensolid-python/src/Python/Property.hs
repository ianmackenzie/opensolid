module Python.Property (definition) where

import API.Property (Property (..))
import API.Property qualified as Property
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: FFI.ClassName -> (Name, Property) -> Text
definition className (propertyName, property) = do
  let ffiFunctionName = Property.ffiName className propertyName
  let selfType = FFI.Class className
  let returnType = Property.returnType property
  Python.lines
    [ "@cached_property"
    , "def "
        <> FFI.snakeCase propertyName
        <> "(self) ->"
        <> Python.Type.qualifiedName returnType
        <> ":"
    , Python.indent
        [ Python.docstring (Property.documentation property)
        , Python.Function.body ffiFunctionName [("self", selfType)] returnType
        ]
    ]
