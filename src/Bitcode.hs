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
   = Instruction
     {
         location :: Location,
         instructionContent :: InstructionContent
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data InstructionContent
   = Nop
   | Call CallContent
   | Unop UnopContent
   | Binop BinopContent
   | AssumeCtor Assume
   | Return ReturnContent
   | Assign AssignContent
   | LoadImm LoadImmContent
   | ParamDecl ParamDeclContent
   | FieldRead FieldReadContent
   | FieldWrite FieldWriteContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

mkNopInstruction :: Location -> Instruction
mkNopInstruction l = Instruction { location = l, instructionContent = Nop }

data TmpVariable
   = TmpVariable
     {
         tmpVariableLocation :: Location,
         tmpVariableSerialIdx :: Integer
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data SrcVariable
   = SrcVariable
     {
         srcVariableToken :: Token.VarName
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data Variable
   = TmpVariableCtor TmpVariable
   | SrcVariableCtor SrcVariable
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

locationVariable :: Variable -> Location
locationVariable v = case v of
    (TmpVariableCtor tmpVariable) -> tmpVariableLocation tmpVariable
    (SrcVariableCtor srcVariable) -> Token.getSrcVariableLocation $ srcVariableToken srcVariable

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

data Assume
   = Assume
     {
         assumeTmpVariable :: TmpVariable,
         assumedValue :: Bool
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

mkAssumeInstruction :: TmpVariable -> Bool -> Instruction
mkAssumeInstruction tmpVariable value = Instruction { location = l, instructionContent = assume }
    where
        l = tmpVariableLocation tmpVariable
        assume = AssumeCtor $ Assume { assumeTmpVariable = tmpVariable, assumedValue = value }

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


