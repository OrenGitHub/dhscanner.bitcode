-- |
--
-- * The [intermediate language (IL) \/ intermediate representation (IR) \/ bitcode](https://en.wikipedia.org/wiki/Intermediate_representation#Intermediate_language)
-- are all synonyms for:
--
--     * a data structure able to represent code originating from /multiple/ programming languages.
--     * minimal instruction set, similar in spirit to [RISC](https://en.wikipedia.org/wiki/Reduced_instruction_set_computer) architectures
--     * unlike /actual/ assembly, it has an /infinite/ number of temporaries ( instead of registers )
--
-- * Its main purpose is to serve as the:
--
--     * second step for /static code analysis/ 
--     * part of the [dhscanner](https://github.com/OrenGitHub/dhscanner) framework for
--       static analysis performing security checks 🔒 and
--       [PII](https://en.wikipedia.org/wiki/Personal_data) leaks detection 🪪
--
-- * As part of the [dhscanner](https://github.com/OrenGitHub/dhscanner) framework:
--
--     * targets mostly languages used for /cloud native applications/ ☁️
--     * Python, Ruby 💎, Php, Javascript, Typescript, Java ☕️, C# and Golang.
--
-- * Typical flow:
--
--     * Abstract syntax trees (ASTs) are scanned
--
--         * 'Callable' entities are identified
--         * each 'Callable' is associated with its control flow graph ('Cfg')
--         * control flow graphs are directed, connecting bitcode instructions
--
-- * Non Haskell parogrammers note:
--
--     * Each 'Callable' object is /immutable/ ( like everything else in Haskell ... )
--

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
-- * That is also true for instrumented instructions (like `Nop` and `Assume`)
--
data Instruction
   = Instruction
     {
         location :: Location,
         instructionContent :: InstructionContent
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

-- |
-- A minimal instruction set for translating common programming languages
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
   | LoadImmNull NullContent
   | ParamDecl ParamDeclContent
   | FieldRead FieldReadContent
   | FieldWrite FieldWriteContent
   | UnresolvedRef UnresolvedRefContent -- ^ @since 1.0.5
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

data ParamVariable
   = ParamVariable
     {
         paramVariableFqn :: Fqn,
         paramVariableSerialIdx :: Word, -- ^ zero-based
         paramVariableToken :: Token.ParamName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Variable
   = TmpVariableCtor TmpVariable
   | SrcVariableCtor SrcVariable
   | ParamVariableCtor ParamVariable
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
    (ParamVariableCtor _paramVariable) -> Token.getParamNameLocation $ paramVariableToken _paramVariable

data CallContent
   = CallContent
     {
         callOutput :: Variable,
         callee :: Variable,
         args :: [ Variable ],
         callLocation :: Location
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data UnresolvedRefContent
   = UnresolvedRefContent
     {
         unresolvedRefOutput :: Variable,
         unresolvedRef :: Variable,
         unresolvedRefLocation :: Location
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
         assumeVariable :: Variable,
         assumedValue :: Bool
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

mkAssumeInstruction :: Variable -> Bool -> Instruction
mkAssumeInstruction v b = Instruction (locationVariable v) (Assume (AssumeContent v b))

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

data NullContent
   = NullContent
     {
         loadImmNullOutput :: TmpVariable,
         loadImmNullValue :: Token.ConstNull
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


-- |
--
-- * some instructions don't have an output variable
--
--     * 'Nop', 'Assume', 'Return' etc.
--
-- * other instructions have /exactly one/ output variable
--
output :: InstructionContent -> Maybe Variable
output (Unop          c) = Just $ unopOutput          c
output (Call          c) = Just $ callOutput          c
output (Binop         c) = Just $ binopOutput         c
output (Assign        c) = Just $ assignOutput        c
output (FieldRead     c) = Just $ fieldReadOutput     c
output (SubscriptRead c) = Just $ subscriptReadOutput c
output _              = Nothing

-- |
--
-- * some instructions don't have input variables /at all/
--
-- * other instructions have /multiple/ input variables
--
inputs :: InstructionContent -> Set Variable
inputs (Call       c) = Data.Set.fromList (args c)
inputs              _ = Data.Set.empty

variables :: InstructionContent -> Set Variable
variables instruction = case output instruction of
    Nothing -> inputs instruction
    Just oneOutput -> (Data.Set.singleton oneOutput) `Data.Set.union` (inputs instruction)

