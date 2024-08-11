-- |
--
-- * The [intermediate language (IL) \/ intermediate representation (IR) \/ bitcode](https://en.wikipedia.org/wiki/Intermediate_representation#Intermediate_language)
-- are all synonyms for a data structure able to represent code originating from /multiple/
-- programming languages.
--
-- * In our context, the main purpose is to enable an efficient and uniform /static code analysis/,
-- as part of the [dhscanner](https://github.com/OrenGitHub/dhscanner) framework
-- for [CI/CD](https://en.wikipedia.org/wiki/CI/CD) container security checks.
--
-- * [dhscanner](https://github.com/OrenGitHub/dhscanner) targets mostly languages used for /cloud native applications/:
-- __Python__, __Ruby__, __Php__, __Javascript__, __Typescript__, __Java__, __C#__ and __Golang__.
--
-- * Typically, a collection of files are first parsed using relevant
-- [parsers](https://github.com/OrenGitHub/dhscanner.1.parsers), then,
-- the resulting abstract syntax trees are sent to code generation,
-- where they are translated into a collection of /callables/.
--
-- * A callable is a sequence of commands corresponding to either a function, a method,
-- a lambda or (in languages like python) a script.
--

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
import Data.Aeson
import GHC.Generics

data Callables
   = Callables
     {
         actualCallables :: [ Callable ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- | Four kinds of callables.
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
         hostingClassSupers :: [ Token.SuperName ],
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

data Annotation
   = Annotation
     {
         annotationFqn :: String,
         annotationConstantStrings :: [ String ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data FunctionContent
   = FunctionContent
     {
         funcName :: Token.FuncName,
         funcBody :: Cfg,
         funcAnnotations :: [ Annotation ],
         funcLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

