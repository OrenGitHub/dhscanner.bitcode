{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Callable

where

-- project imports
import Cfg
import Fqn
import Token
import Location

-- general imports
import Data.Set
import Data.Aeson
import GHC.Generics

data Callables
   = Callables
     {
         actualCallables :: [ Callable ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Callable
   = Method MethodContent
   | Lambda LambdaContent
   | Script ScriptContent
   | Function FunctionContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data MethodContent
   = MethodContent
     {
         methodName :: Token.MethdName,
         hostingClassName :: Token.ClassName,
         methodBody :: Cfg,
         methodLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data LambdaContent
   = LambdaContent
     {
         lambdaBody :: Cfg,
         lambdaLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ScriptContent
   = ScriptContent
     {
         filename :: String,
         scriptBody :: Cfg
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data FunctionContent
   = FunctionContent
     {
         funcName :: Token.FuncName,
         funcBody :: Cfg,
         funcLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

