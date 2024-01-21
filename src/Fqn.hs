module Fqn
where
data Fqn
   = Fqn
     {
         content :: String
     }
     deriving ( Show, ToJSON, FromJSON )