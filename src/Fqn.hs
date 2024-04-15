{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Fqn

where

-- project imports
import ActualType

-- general imports
import Data.Aeson
import GHC.Generics

-- | Fully qualified name
data Fqn = Fqn { content :: String } deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

fromActualType :: ActualType -> Fqn
fromActualType (ThirdPartyImport (ThirdPartyImportContent name)) = Fqn name
fromActualType _ = nativeInt

nativeInt :: Fqn
nativeInt = Fqn { content = "int" }

nativeStr :: Fqn
nativeStr = Fqn { content = "str" }

convertFrom :: ActualType -> Fqn
convertFrom  = Fqn . show
