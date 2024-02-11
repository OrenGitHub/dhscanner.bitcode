{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Cfg

where

-- project imports
import Location
import qualified Bitcode

-- general imports
import Data.Aeson
import GHC.Generics hiding ( from, to )
import Prelude hiding ( filter, map )
import Data.Set ( Set, fromList, filter, map, union )

data Node
   = Node
     {
         theInstructionInside :: Bitcode.Instruction
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Nodes
   = Nodes
     {
         actualNodes :: Set Node
     }
     deriving ( Show, Eq, Ord )

data Edge
   = Edge
     {
         from :: Node,
         to   :: Node
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Edges
   = Edges
     {
         actualEdges :: Set Edge
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

mkEmptyCollectionOfEdges :: Edges
mkEmptyCollectionOfEdges = Edges { actualEdges = fromList [] }

data Cfg
   = Cfg
     {
         entry :: Node,
         exit  :: Node,
         location :: Location,
         edges :: Edges
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

nodes :: Cfg -> Nodes
nodes g = Nodes { actualNodes = nodes' `union` nodes'' }
    where
        nodes'  = map from (actualEdges (edges g))
        nodes'' = map to   (actualEdges (edges g))

preds :: Node -> Cfg -> Nodes
preds node g = Nodes { actualNodes = map from edges' }
    where
        edges' = filter (\e -> (to e) == node) (actualEdges (edges g))    

atom :: Node -> Cfg
atom node = Cfg { entry = node, exit = node, location = l, edges = edges' }
    where
        edges' = mkEmptyCollectionOfEdges
        l = Bitcode.location (theInstructionInside node)

concat :: Cfg -> Cfg -> Location -> Cfg
concat g1 g2 l = Cfg { entry = entry g1, exit = exit g2, location = l, edges = edges' }
    where
        edges' = Edges $ edges1 `union` edges2 `union` connector
            where
                edges1 = actualEdges $ edges g1
                edges2 = actualEdges $ edges g2
                connector = fromList [Edge { from = exit g1, to = entry g2 }]

parallel :: Cfg -> Cfg -> Location -> Cfg
parallel g1 g2 l = Cfg { entry = s, exit = t, location = l, edges = edges' }
    where
        s = Node $ Bitcode.mkNopInstruction l
        t = Node $ Bitcode.mkNopInstruction l
        edges' = Edges $ edges1 `union` edges2 `union` connectors
            where
                edges1 = actualEdges $ edges g1
                edges2 = actualEdges $ edges g2
                connectors = fromList [ s_g1, s_g2, g1_t, g2_t ]
                    where
                        s_g1 = Edge { from = s, to = entry g1 }
                        s_g2 = Edge { from = s, to = entry g2 }
                        g1_t = Edge { from = entry g1, to = t }
                        g2_t = Edge { from = entry g2, to = t }

-- | create a loop from condition and body
loopify :: Cfg -> Cfg -> Bitcode.TmpVariable -> Location -> Cfg
loopify cond body guardedValue l = Cfg { entry = entry cond, exit = t, location = l, edges = edges' }
    where
        t = Node $ Bitcode.mkNopInstruction l
        edges' = Edges $ edges1 `union` edges2 `union` connectors
            where
                edges1 = actualEdges $ edges cond
                edges2 = actualEdges $ edges body
                connectors = fromList [ e1, e2, e3, e4, e5 ]
                    where
                        e1 = Edge { from = exit cond, to = Node $ Bitcode.mkAssumeInstruction guardedValue True }
                        e2 = Edge { from = exit cond, to = Node $ Bitcode.mkAssumeInstruction guardedValue False }
                        e3 = Edge { from = Node $ Bitcode.mkAssumeInstruction guardedValue True, to = entry body }
                        e4 = Edge { from = Node $ Bitcode.mkAssumeInstruction guardedValue False, to = t }
                        e5 = Edge { from = exit body, to = entry cond }
