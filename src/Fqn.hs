{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Fqn
where

import Data.Aeson
import GHC.Generics

data Fqn
   = Fqn
     {
         content :: String
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )
