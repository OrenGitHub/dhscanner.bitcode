{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Cfg

where

-- project imports
import qualified Bitcode

-- general imports
import Data.Aeson
import GHC.Generics hiding ( from, to )
import Prelude hiding ( filter, map )
import Data.Set ( Set, fromList, map, union, empty, singleton )

data Node = Node { theInstructionInside :: Bitcode.Instruction } deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Nodes = Nodes { actualNodes :: Set Node } deriving ( Show, Eq, Ord )

data Edge = Edge { from :: Node, to :: Node } deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Edges = Edges { actualEdges :: Set Edge } deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

emptySetOfNodes :: Nodes
emptySetOfNodes = Nodes { actualNodes = empty }

emptySetOfEdges :: Edges
emptySetOfEdges = Edges { actualEdges = empty }

data Cfg = Empty | Normal Content deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Content = Content { entry :: Node, exit  :: Node, edges :: Edges } deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

nodes :: Cfg -> Nodes
nodes Empty = emptySetOfNodes
nodes (Normal cfg) = nodes' (edges cfg)

nodes' :: Edges -> Nodes
nodes' = nodes'' . actualEdges

nodes'' :: Set Edge -> Nodes
nodes'' edges' = Nodes { actualNodes = (map from edges') `union` (map to edges') }

atom :: Node -> Content
atom node = Content { entry = node, exit = node, edges = emptySetOfEdges }

concat :: Cfg -> Cfg -> Cfg
concat Empty cfg = cfg
concat (Normal content) cfg = Normal (concatNormalContentFirst content cfg)

concatNormalContentFirst :: Content -> Cfg -> Content
concatNormalContentFirst content Empty = content
concatNormalContentFirst cfg1 (Normal cfg2) = let
    edges1 = actualEdges (edges cfg1)
    edges2 = actualEdges (edges cfg2)
    connector = singleton (Edge { from = exit cfg1, to = entry cfg2 })
    edges12 = Edges (edges1 `union` edges2 `union` connector)
    in Content { entry = entry cfg1, exit = exit cfg2, edges = edges12 }

mkNopNode :: Node -> Node
mkNopNode = Node . Bitcode.mkNopInstruction . Bitcode.location . theInstructionInside

mkNewEntry :: Node -> Node
mkNewEntry = mkNopNode

mkNewExit :: Node -> Node
mkNewExit = mkNopNode

parallelNormalCfgs :: Content -> Content -> Cfg
parallelNormalCfgs cfg1 cfg2 = let
    s = mkNewEntry (entry cfg1)
    t = mkNewExit (exit cfg2)
    c1 = Edge { from = s, to = entry cfg1 }
    c2 = Edge { from = s, to = entry cfg2 }
    c3 = Edge { from = exit cfg1, to = t }
    c4 = Edge { from = exit cfg2, to = t }
    edges1 = actualEdges (edges cfg1)
    edges2 = actualEdges (edges cfg2)
    connectors = fromList [ c1, c2, c3, c4 ]
    edges12 = Edges (edges1 `union` edges2 `union` connectors)
    in Normal (Content { entry = s, exit = t, edges = edges12 })

loopifyEmptyCondNormalBody :: Content -> Bitcode.Value -> Cfg
loopifyEmptyCondNormalBody _body _guardedValue = Empty

loopifyNormalCondEmptyBody :: Content -> Bitcode.Value -> Cfg
loopifyNormalCondEmptyBody _cond _guardedValue = Empty

loopify :: Cfg -> Cfg -> Bitcode.Value -> Cfg
loopify Empty Empty _ = Empty
loopify Empty (Normal body) guardedValue = loopifyEmptyCondNormalBody body guardedValue
loopify (Normal cond) Empty guardedValue = loopifyNormalCondEmptyBody cond guardedValue
loopify (Normal cond) (Normal body) guardedValue = let
    guardedValueIsTrue = Node (Bitcode.mkAssumeInstruction guardedValue True)
    guardedValueIsFalse = Node (Bitcode.mkAssumeInstruction guardedValue False)
    c1 = Edge { from = exit cond, to = guardedValueIsTrue }
    c2 = Edge { from = exit cond, to = guardedValueIsFalse }
    c3 = Edge { from = guardedValueIsTrue, to = entry body }
    c4 = Edge { from = guardedValueIsFalse, to = exit body }
    c5 = Edge { from = exit body, to = entry cond }
    edgesCond = actualEdges (edges cond)
    edgesBody = actualEdges (edges body)
    connectors = fromList [ c1, c2, c3, c4, c5 ]
    edges' = Edges (edgesCond `union` edgesBody `union` connectors)
    in Normal (Content { entry = entry cond, exit = exit body, edges = edges' })
