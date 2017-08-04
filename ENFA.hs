module ENFA where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Input a = Symbol a
             | Eps
             | Any
             deriving (Eq,Ord,Show)

type Delta  s a = (s, Input a, s)
type FDelta s a = Map (Input a) (Set s)

data ENFA s a = ENFA
     { start         :: s
     , delta         :: Map s (FDelta s a)
     , accepting     :: Set s
     } deriving Show

initENFA :: s -> ENFA s a
initENFA q0 = ENFA q0 Map.empty Set.empty

trans :: (Ord a, Ord s) => Delta s a -> ENFA s a -> ENFA s a
trans (q,i,q') (ENFA q0 ts fs) = ENFA q0 (insert ts) fs
  where insert = Map.insertWith (Map.unionWith Set.union) q
                 (Map.singleton i (Set.singleton q'))

accept :: (Ord a, Ord s) => s -> ENFA s a -> ENFA s a
accept q (ENFA q0 ts fs) = ENFA q0 ts (Set.insert q fs)
