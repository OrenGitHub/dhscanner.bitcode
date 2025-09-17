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
import GHC.Generics
import Data.Aeson ( ToJSON, FromJSON )

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
   | ParamDecl ParamDeclContent
   | FieldRead FieldReadContent
   | FieldWrite FieldWriteContent
   | UnresolvedRef UnresolvedRefContent -- ^ @since 1.0.5
   | SubscriptRead SubscriptReadContent
   | SubscriptWrite SubscriptWriteContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data Value
   = VariableCtor Variable
   | ConstValueCtor ConstValue
   | KeywordArgCtor KeywordArgVariable
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Variable
   = TmpVariableCtor TmpVariable
   | SrcVariableCtor SrcVariable
   | ParamVariableCtor ParamVariable
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

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

data KeywordArgVariable
   = KeywordArgVariable
     {
         keywordArgName :: String,
         keywordArgValue :: Value
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

variableFqn :: Variable -> Fqn
variableFqn (TmpVariableCtor   (TmpVariable   fqn _   )) = fqn
variableFqn (SrcVariableCtor   (SrcVariable   fqn _   )) = fqn
variableFqn (ParamVariableCtor (ParamVariable fqn _ _ )) = fqn

locationVariable :: Variable -> Location
locationVariable (TmpVariableCtor t) = tmpVariableLocation t
locationVariable (SrcVariableCtor s) = Token.getVarNameLocation (srcVariableToken s)
locationVariable (ParamVariableCtor p) = Token.getParamNameLocation (paramVariableToken p)

locationValue :: Value -> Location
locationValue (VariableCtor v) = locationVariable v
locationValue (ConstValueCtor c) = locationConstValue c
locationValue (KeywordArgCtor k) = locationValue (keywordArgValue k)

locationConstValue :: ConstValue -> Location
locationConstValue (ConstIntValue  i) = Token.constIntLocation  i
locationConstValue (ConstStrValue  s) = Token.constStrLocation  s
locationConstValue (ConstBoolValue b) = Token.constBoolLocation b
locationConstValue (ConstNullValue n) = Token.constNullLocation n

data CallContent
   = CallContent
     {
         callOutput :: Variable,
         callee :: Variable,
         args :: [ Value ],
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
         binopLhs :: Value,
         binopRhs :: Value
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data UnopContent
   = UnopContent
     {
         unopOutput :: Variable,
         unopLhs :: Value
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data AssumeContent
   = AssumeContent
     {
         assumeValue :: Value,
         assumeTruthy :: Bool
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

mkNopInstruction :: Location -> Instruction
mkNopInstruction l = Instruction { location = l, instructionContent = Nop }

mkAssumeInstruction :: Value -> Bool -> Instruction
mkAssumeInstruction v b = Instruction (locationValue v) (Assume (AssumeContent v b))

data ReturnContent
   = ReturnContent
     {
         returnValue :: Maybe Value
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data AssignContent
   = AssignContent
     {
         assignOutput :: Variable,
         assignInput :: Value
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data ConstValue
   = ConstIntValue Token.ConstInt
   | ConstStrValue Token.ConstStr
   | ConstBoolValue Token.ConstBool
   | ConstNullValue Token.ConstNull
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
         fieldWriteInput :: Value
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data SubscriptReadContent
   = SubscriptReadContent
     {
         subscriptReadOutput :: Variable,
         subscriptReadInput :: Variable,
         subscriptReadIdx :: Value
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data SubscriptWriteContent
   = SubscriptWriteContent
     {
         subscriptWriteOutput :: Variable,
         subscriptWriteIdx :: Value,
         subscriptWriteInput :: Value
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data ParamDeclContent
   = ParamDeclContent
     {
         paramVariable :: ParamVariable
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
