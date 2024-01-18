{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcode

where

import Location
import qualified Token

import Data.Aeson
import GHC.Generics

data Instruction
   = InstructionNop NopContent
   | InstructionCall CallContent
   | InstructionUnop UnopContent
   | InstructionBinop BinopContent
   | InstructionAssume AssumeContent
   | InstructionReturn ReturnContent
   | InstructionAssign AssignContent
   | InstructionLoadImm LoadImmContent
   | InstructionParamDecl ParamDeclContent
   | InstructionFieldRead FieldReadContent
   | InstructionFieldWrite FieldWriteContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data TmpVariableContent
   = TmpVariableContent
     {
         tmpVariableLocation :: Location,
         tmpVariableSerialIdx :: Integer
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data SrcVariableContent
   = SrcVariableContent
     {
         srcVariableName :: Token.VarName
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data Variable
   = TmpVariable TmpVariableContent
   | SrcVariable SrcVariableContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data NopContent
   = NopContent
     {
         nopContentLocation :: Location
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data Arg
   = ArgPlain Integer
   | ArgKeyword Integer
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord ) 

data CallContent
   = CallContent
     {
         callOutput :: Variable,
         callee :: Variable,
         args :: [ Arg ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data BinopContent
   = BinopContent
     {
         binopOutput :: Variable,
         binopLhs :: Variable,
         binopRhs :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data UnopContent
   = UnopContent
     {
         unopOutput :: Variable,
         unopLhs :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data AssumeContent
   = AssumeContent
     {
         assumedVariable :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data ReturnContent
   = ReturnContent
     {
         returnValue :: Maybe Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data AssignContent
   = AssignContent
     {
         assignOutput :: Variable,
         assignInput :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data LoadImmContent
   = LoadImmContentInt Integer
   | LoadImmContentStr String
   | LoadImmContentBool Bool
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data FieldReadContent
   = FieldReadContent
     {
         fieldReadOutput :: Variable,
         fieldReadInput :: Variable,
         fieldReadName :: Token.FieldName
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data FieldWriteContent
   = FieldWriteContent
     {
         fieldWriteOutput :: Variable,
         fieldWriteName :: Token.FieldName,
         fieldWriteInput :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data ParamDeclContent
   = ParamDeclContent
     {
         paramDeclName :: Token.ParamName,
         paramDeclSerialIdx :: Integer
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )


