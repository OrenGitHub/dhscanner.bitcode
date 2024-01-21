module Cfg

where

import qualified Bitcode
import Prelude hiding ( filter, map )
import Data.Set ( Set, fromList, filter, map )

data Node
   = Node
     {
         instruction :: Bitcode.Instruction
     }
     deriving ( Show, Ord, Eq )

data Nodes
   = Nodes
     {
         actualNodes :: Set Node
     }
     deriving ( Show )

data Edge
   = Edge
     {
         from :: Node,
         to   :: Node
     }
     deriving ( Show )

data Edges
   = Edges
     {
         actualEdges :: Set Edge
     }
     deriving ( Show )

mkEmptyCollectionOfEdges :: Edges
mkEmptyCollectionOfEdges = Edges { actualEdges = fromList [] }

data Cfg
   = Cfg
     {
         entry :: Node,
         exit  :: Node,
         edges :: Edges
     }
     deriving ( Show )

nodes :: Cfg -> Nodes
nodes g = Nodes { actualNodes = (map from (edges g)) `union` (map to (edges g)) }

preds :: Node -> Cfg -> Nodes
preds node g = Nodes { actualNodes = map from edges' }
    where
        edges' = filter (\e -> (to e) == node) (actualEdges (edges g))    

atom :: Node -> Cfg
atom node = Cfg { entry = node, exit = node, edges = edges' }
    where edges' = mkEmptyCollectionOfEdges

concat :: Cfg -> Cfg -> Cfg
concat g1 g2 = Cfg { entry = entry g1, exit = exit g2, edges = edges' }
    where
        edges' = Edges $ edges1 `union` edges2 `union` connector
            where
                edges1 = actualEdges $ edges g1
                edges2 = actualEdges $ edges g2
                connector = fromList [Edge { from = exit g1, to = entry g2 }]

parallel :: Cfg -> Cfg -> Cfg
parallel g1 g2 = Cfg { entry = s, exit = t, edges = edges' }
    where
        s = Bitcode.Nop
        t = Bitcode.Nop
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
loopify :: Cfg -> Cfg -> Bitcode.TmpVariable -> Cfg
loopify cond body guardedValue = Cfg { entry = s, exit = t, edges = edges' }
    where
        s = Bitcode.Nop
        t = Bitcode.Nop
        edges' = Edges $ edges1 `union` edges2 `union` connectors
            where
                edges1 = actualEdges $ edges cond
                edges2 = actualEdges $ edges body
                connectors = fromList [
                    Edge { from = exit cond, to = guardedValueIsTrue },
                    Edge { from = exit cond, to = guardedValueIsFalse }
                    Edge { from = guardedValueIsTrue, to = entry body },
                    Edge { from = guardedValueIsFalse, to = t },
                    Edge { from = exit body, to = entry cond }
                ]
                    where
                        guardedValueIsTrue = Bitcode.Assume guardedValue True                       
                        guardedValueIsFalse = Bitcode.Assume guardedValue False
