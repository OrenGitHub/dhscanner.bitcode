module Callable

where

import Cfg
import Fqn

data Callable
   = Callable
     {
         fqn :: Fqn,
         cfg :: Cfg
     }
     deriving ( Show, ToJSON, FromJSON )