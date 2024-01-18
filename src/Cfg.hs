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

data Cfg
   = Cfg
     {
         entry :: Node,
         exit  :: Node,
         edges :: Edges
     }
     deriving ( Show )

preds :: Node -> Cfg -> Nodes
preds node g = let
    edges' = filter (\e -> (to e) == node) (actualEdges (edges g))
    in
    Nodes { actualNodes = map from edges' }

