{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Callable

where

import Cfg
import Fqn

import Data.Aeson
import GHC.Generics

data Callable
   = Callable
     {
         fqn :: Fqn,
         cfg :: Cfg
     }
     deriving ( Show, Generic, ToJSON, FromJSON )

data Callables
   = Callables
     {
         actualCallables :: [ Callable ]
     }
     deriving ( Show, Generic, ToJSON, FromJSON )
