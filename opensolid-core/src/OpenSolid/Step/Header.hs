module OpenSolid.Step.Header
  ( Header
  , description
  , implementationLevel
  , fileName
  , timestamp
  , author
  , organization
  , preprocessorVersion
  , originatingSystem
  , authorization
  , schemaIdentifiers
  )
where

import OpenSolid.Prelude
import OpenSolid.Step.Types (Header (..))

description :: Header -> List Text
description = (.description)

implementationLevel :: Header -> Text
implementationLevel = (.implementationLevel)

fileName :: Header -> Text
fileName = (.fileName)

timestamp :: Header -> Text
timestamp = (.timestamp)

author :: Header -> List Text
author = (.author)

organization :: Header -> List Text
organization = (.organization)

preprocessorVersion :: Header -> Text
preprocessorVersion = (.preprocessorVersion)

originatingSystem :: Header -> Text
originatingSystem = (.originatingSystem)

authorization :: Header -> Text
authorization = (.authorization)

schemaIdentifiers :: Header -> List Text
schemaIdentifiers = (.schemaIdentifiers)
