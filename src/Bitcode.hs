{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcode

where

-- project imports
import Fqn
import Location
import qualified Token

-- general imports
import Data.Aeson
import GHC.Generics
import Data.Set ( Set )

-- general (qualified) imports
import qualified Data.Set

-- |
-- * All instructions have an associated location
--
-- * That is true also for instrumented instructions (like `Nop` and `Assume`)
--
data Instruction
   = Instruction
     {
         location :: Location,
         instructionContent :: InstructionContent
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

-- | A minimal instruction set to translate /any/ programming language
-- to an intermediate langauge ready for static analysis
data InstructionContent
   = Nop
   | Call CallContent
   | Unop UnopContent
   | Binop BinopContent
   | Assume AssumeContent
   | Return ReturnContent
   | Assign AssignContent
   | LoadImmStr StrContent
   | LoadImmInt IntContent
   | LoadImmBool BoolContent
   | ParamDecl ParamDeclContent
   | FieldRead FieldReadContent
   | FieldWrite FieldWriteContent
   | SubscriptRead SubscriptReadContent
   | SubscriptWrite SubscriptWriteContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

mkNopInstruction :: Location -> Instruction
mkNopInstruction l = Instruction { location = l, instructionContent = Nop }

data TmpVariable
   = TmpVariable
     {
         tmpVariableFqn :: Fqn,
         tmpVariableLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data SrcVariable
   = SrcVariable
     {
         srcVariableFqn :: Fqn,
         srcVariableToken :: Token.VarName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ArgContent
   = ArgContent
     {
         argVariableFqn :: Fqn,
         argVariableSerialIdx :: Word,
         -- location is indicative to the call
         -- this arg was generated for
         argVariableMyAwesomeCallContext :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ParamVariable
   = ParamVariable
     {
         paramVariableFqn :: Fqn,
         paramVariableSerialIdx :: Word,
         paramVariableToken :: Token.ParamName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Variable
   = TmpVariableCtor TmpVariable
   | SrcVariableCtor SrcVariable
   | ParamVariableCtor ParamVariable
   | Arg ArgContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

variableFqn :: Variable -> Fqn
variableFqn (TmpVariableCtor   (TmpVariable   fqn _   )) = fqn
variableFqn (SrcVariableCtor   (SrcVariable   fqn _   )) = fqn
variableFqn (ParamVariableCtor (ParamVariable fqn _ _ )) = fqn
variableFqn _ = Fqn "blah"

-- | Can /not/ be serialized to JSON
data Variables = Variables { actualVariables :: Set Variable } deriving ( Show, Eq, Ord )

data SrcVariables = SrcVariables { actualSrcVariables :: Set SrcVariable } deriving ( Show, Eq, Ord )

-- | Creating an empty collection of global variables
createEmptyCollectionOfGlobalVariables :: SrcVariables
createEmptyCollectionOfGlobalVariables = SrcVariables { actualSrcVariables = Data.Set.empty }
 
-- | Can /not/ be serialized to JSON
data TmpVariables = TmpVariables { actualTmpVariables :: Set TmpVariable } deriving ( Show, Eq, Ord )

locationVariable :: Variable -> Location
locationVariable v = case v of
    (TmpVariableCtor tmpVariable) -> tmpVariableLocation tmpVariable
    (SrcVariableCtor srcVariable) -> Token.getVarNameLocation $ srcVariableToken srcVariable
    (ParamVariableCtor paramVariable) -> Token.getParamNameLocation $ paramVariableToken paramVariable

data CallContent
   = CallContent
     {
         callOutput :: Variable,
         callee :: Variable,
         args :: [ Variable ],
         callLocation :: Location
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

callInputs :: CallContent -> [ TmpVariable ]
callInputs callContent = []

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
         unopOutput :: TmpVariable,
         unopLhs :: TmpVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data AssumeContent
   = AssumeContent
     {
         assumeVariable :: Variable,
         assumedValue :: Bool
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

mkAssumeInstruction :: Variable -> Bool -> Instruction
mkAssumeInstruction v b = Instruction (locationVariable v) (Assume (AssumeContent v b))

data ReturnContent
   = ReturnContent
     {
         returnValue :: Maybe TmpVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data AssignContent
   = AssignContent
     {
         assignOutput :: Variable,
         assignInput :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data IntContent
   = IntContent
     {
         loadImmIntOutput :: TmpVariable,
         loadImmIntValue :: Token.ConstInt
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data StrContent
   = StrContent
     {
         loadImmStrOutput :: TmpVariable,
         loadImmStrValue :: Token.ConstStr
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data BoolContent
   = BoolContent
     {
         loadImmBoolOutput :: TmpVariable,
         loadImmBoolValue :: Token.ConstBool
     }
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

data SubscriptReadContent
   = SubscriptReadContent
     {
         subscriptReadOutput :: Variable,
         subscriptReadInput :: Variable,
         subscriptReadIdx :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data SubscriptWriteContent
   = SubscriptWriteContent
     {
         subscriptWriteOutput :: Variable,
         subscriptWriteIdx :: Variable,
         subscriptWriteInput :: Variable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data ParamDeclContent
   = ParamDeclContent
     {
         paramVariable :: ParamVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )


-- some instructions don't have an output variable
-- at any case, there is at most one output (unlike inputs)
output :: InstructionContent -> Maybe Variable
output (Unop       c) = Just $ TmpVariableCtor $ unopLhs          c
output (Assign     c) = Just $                   assignOutput     c
output _              = Nothing

-- some instructions don't have an input variable
-- there can be /multiple/ input variables to an instruction
inputs :: InstructionContent -> Set TmpVariable
inputs (Call       c) = Data.Set.fromList  $ callInputs       c
inputs (Unop       c) = Data.Set.singleton $ unopLhs          c
inputs              _ = Data.Set.empty

inputs' :: InstructionContent -> Set Variable
inputs' = (Data.Set.map TmpVariableCtor) . inputs

variables :: InstructionContent -> Set Variable
variables instruction = case output instruction of
    Nothing -> inputs' instruction
    Just output' -> (Data.Set.singleton output') `Data.Set.union` (inputs' instruction)

