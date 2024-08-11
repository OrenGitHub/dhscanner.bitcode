{-# LANGUAGE DeriveGeneric #-}

module Main

where

-- project imports
import Cfg
import Fqn
import Token
import Bitcode
import Location

-- general imports
import Data.Set
import Data.List
import System.Exit
import Test.QuickCheck

prop_nonAtomCfgsHaveEdges :: Cfg -> Cfg -> Bool
prop_nonAtomCfgsHaveEdges cfg1 cfg2 = let cfg3 = Cfg.concat cfg1 cfg2 in (length (toList (actualEdges (edges cfg3)))) >= 1

instance Arbitrary Cfg
    where arbitrary = Cfg <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Node
    where arbitrary = Node <$> arbitrary

instance Arbitrary Instruction
    where arbitrary = Instruction <$> arbitrary <*> arbitrary

instance Arbitrary InstructionContent
    where arbitrary = LoadImmInt <$> arbitrary

instance Arbitrary IntContent
    where arbitrary = IntContent <$> arbitrary <*> arbitrary

instance Arbitrary ConstInt
    where arbitrary = ConstInt <$> arbitrary <*> arbitrary

instance Arbitrary TmpVariable
    where arbitrary = TmpVariable <$> arbitrary <*> arbitrary

instance Arbitrary Fqn
    where arbitrary = Fqn <$> arbitrary

instance Arbitrary Edges
    where arbitrary = Edges <$> arbitrary

instance Arbitrary Edge
    where arbitrary = Edge <$> arbitrary <*> arbitrary

instance Arbitrary Location
    where arbitrary = Location <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

main :: IO ()
main = do
    result <- quickCheckResult prop_nonAtomCfgsHaveEdges
    case (isSuccess result) of { True -> exitSuccess; False -> exitFailure }
