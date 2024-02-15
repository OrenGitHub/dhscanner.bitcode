{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcode

where

-- project imports
import Location
import ActualType
import qualified Token

-- general imports
import Data.Aeson
import GHC.Generics
import Data.Set ( Set )

-- general (qualified) imports
import qualified Data.Set

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
   | Assume AssumeContent
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
         actualType :: ActualType,
         tmpVariableLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data SrcVariable
   = SrcVariable
     {
         srcVariableToken :: Token.VarName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Variable
   = TmpVariableCtor TmpVariable
   | SrcVariableCtor SrcVariable
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Variables
   = Variables
     {
         actualVariables :: Set Variable
     }
     deriving ( Show, Eq, Ord )

data TmpVariables
   = TmpVariables
     {
         actualTmpVariables :: Set TmpVariable
     }
     deriving ( Show, Eq, Ord )

locationVariable :: Variable -> Location
locationVariable v = case v of
    (TmpVariableCtor tmpVariable) -> tmpVariableLocation tmpVariable
    (SrcVariableCtor srcVariable) -> Token.getVarNameLocation $ srcVariableToken srcVariable

data Arg
   = ArgPlain Integer
   | ArgKeyword Integer
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord ) 

data CallContent
   = CallContent
     {
         callOutput :: TmpVariable,
         callee :: TmpVariable,
         args :: [ Arg ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

callInputs :: CallContent -> [ TmpVariable ]
callInputs callContent = []

data BinopContent
   = BinopContent
     {
         binopOutput :: TmpVariable,
         binopLhs :: TmpVariable,
         binopRhs :: TmpVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

binopInputs :: BinopContent -> [ TmpVariable ]
binopInputs binopContent = [ binopLhs binopContent, binopRhs binopContent ]

data UnopContent
   = UnopContent
     {
         unopOutput :: TmpVariable,
         unopLhs :: TmpVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data AssumeContent
   = AssumeContent
     {
         assumeTmpVariable :: TmpVariable,
         assumedValue :: Bool
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

mkAssumeInstruction :: TmpVariable -> Bool -> Instruction
mkAssumeInstruction tmpVariable value = Instruction { location = l, instructionContent = assume }
    where
        l = tmpVariableLocation tmpVariable
        assume = Assume $ AssumeContent { assumeTmpVariable = tmpVariable, assumedValue = value }

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
         assignInput :: TmpVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data LoadImmContent
   = LoadImmContentInt TmpVariable Token.ConstInt
   | LoadImmContentStr TmpVariable String
   | LoadImmContentBool TmpVariable Bool
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data FieldReadContent
   = FieldReadContent
     {
         fieldReadOutput :: TmpVariable,
         fieldReadInput :: TmpVariable,
         fieldReadName :: Token.FieldName
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data FieldWriteContent
   = FieldWriteContent
     {
         fieldWriteOutput :: TmpVariable,
         fieldWriteName :: Token.FieldName,
         fieldWriteInput :: TmpVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data ParamDeclContent
   = ParamDeclContent
     {
         paramDeclName :: Token.ParamName,
         paramDeclSerialIdx :: Integer
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )


-- some instructions don't have an output variable
-- at any case, there is at most one output (unlike inputs)
output :: InstructionContent -> Maybe Variable
output (Call       c) = Just $ TmpVariableCtor $ callOutput       c
output (Unop       c) = Just $ TmpVariableCtor $ unopLhs          c
output (Binop      c) = Just $ TmpVariableCtor $ binopOutput      c
output (Assign     c) = Just $                   assignOutput     c
output (FieldRead  c) = Just $ TmpVariableCtor $ fieldReadOutput  c
output (FieldWrite c) = Just $ TmpVariableCtor $ fieldWriteOutput c
output _              = Nothing

-- some instructions don't have an input variable
-- there can be /multiple/ input variables to an instruction
inputs :: InstructionContent -> Set TmpVariable
inputs (Call       c) = Data.Set.fromList  $ callInputs       c
inputs (Binop      c) = Data.Set.fromList  $ binopInputs      c
inputs (Unop       c) = Data.Set.singleton $ unopLhs          c
inputs (Assign     c) = Data.Set.singleton $ assignInput      c
inputs (FieldRead  c) = Data.Set.singleton $ fieldReadInput   c
inputs (FieldWrite c) = Data.Set.singleton $ fieldWriteOutput c
inputs              _ = Data.Set.empty

inputs' :: InstructionContent -> Set Variable
inputs' = (Data.Set.map TmpVariableCtor) . inputs

variables :: InstructionContent -> Set Variable
variables instruction = case output instruction of
    Nothing -> inputs' instruction
    Just output' -> (Data.Set.singleton output') `Data.Set.union` (inputs' instruction)

