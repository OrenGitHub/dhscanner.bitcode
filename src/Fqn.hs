{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Fqn

where

-- project imports
import Location
import qualified Token

-- general imports
import Data.Aeson
import GHC.Generics

-- |
--
-- * Fully qualified name
-- * Resolution at the /file/ level
-- * Unidirectional resolution only ( one pass )
--
data Fqn
   = NativeTypeInt
   | NativeTypeString
   | NativeTypeBool
   | NativeTypeNull
   | NativeTypeConstInt Int
   | NativeTypeConstStr String
   | NativeTypeConstBool Bool
   | ClassName ClassNameContent
   | ClassInstance ClassInstanceContent
   | FieldedAccess Fqn Token.FieldName
   | CallMethodOfClass Location String Token.ClassName
   | CallMethodOfUntypedNamedParam Location String Token.ParamName
   | CallFuncFromImportedDir Location String FilePath
   | CallFuncFromImportedFile Location String FilePath
   | FirstPartyImport FirstPartyImportContent
   | FirstPartyDirImport FirstPartyDirImportContent
   | ThirdPartyImport ThirdPartyImportContent
   | Unknown
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data FirstPartyImportContent
   = FirstPartyImportContent
     {
         firstPartyImportedLocation :: FilePath,
         firstPartyImportedName :: Maybe String
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data FirstPartyDirImportContent
   = FirstPartyDirImportContent
     {
         firstPartyDirImportLocation :: FilePath,
         firstPartyDirImportAlias :: Maybe String
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- ==== __Examples:__
--
-- * Entire third party package import
--
-- @
-- # thirdPartyImportedPackageName == "urllib3"
-- # thirdPartyImportedPartsFromPackage == []
-- # thirdPartyImportJustOneName == Nothing
-- # thirdPartyImportWithAlias == Nothing
-- import urllib3
-- @
--
-- * Specific third party import
--
-- @
-- # thirdPartyImportedPackageName == "scipy"
-- # thirdPartyImportedPartsFromPackage == [ "linalg" ]
-- # thirdPartyImportJustOneName == Nothing
-- # thirdPartyImportWithAlias == Nothing
-- import scipy.linalg
-- @
--
-- * Named third party import
--
-- @
-- # thirdPartyImportedPackageName == "django"
-- # thirdPartyImportedPartsFromPackage == [ "db" ]
-- # thirdPartyImportJustOneName == "models"
-- # thirdPartyImportWithAlias == Nothing
-- from django.db import models
-- @
--
data ThirdPartyImportContent
   = ThirdPartyImportContent
     {
         thirdPartyImportedPackageName :: String,
         thirdPartyImportedPartsFromPackage :: [ String ],
         thirdPartyImportJustOneName :: Maybe String,
         thirdPartyImportWithAlias :: Maybe String
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ClassInstanceContent
   = ClassInstanceContent
     {
         instanceName :: InstanceName,
         instanceOfClass :: Token.ClassName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data InstanceName
   = Self -- ^ Python, Ruby
   | This -- ^ Java, Php, Javascript, Typescript
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data InstanceAttribute
   = InstanceAttribute
     {
         attributeName :: Token.FieldName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ClassNameContent
   = ClassNameContent
     {
         classNameContent :: Token.ClassName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
