{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Fqn

where

-- general imports
import Data.Aeson
import GHC.Generics

-- | Fully qualified name
data Fqn = Fqn { content :: String } deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

any :: Fqn
any = Fqn { content = "any" }

nativeInt :: Fqn
nativeInt = Fqn { content = "int" }

nativeStr :: Fqn
nativeStr = Fqn { content = "str" }

