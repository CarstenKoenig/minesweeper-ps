module Algorithms.Closure (closure) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set


-- calculates the reflexive/transitive closure of 
-- all a-nodes reachable from start
-- via out-edges produced by the neighbours function
closure :: forall a. Ord a => (a -> Set a) -> a -> Set a
closure neighbours start =
  go Set.empty (Set.singleton start)
  where
  go visited visit =
    -- find a element we are supposed to visit
    -- can be really any, in Set we have the choice
    -- between a minimal or a maximal element
    case Set.findMin visit of
      -- so we have one still to visit
      Just next ->
        let 
          -- we just visited it
          visited' = Set.insert next visited
          -- find all nodes in it's neighbourhood
          ns = neighbours next
          -- we still need to visit the remaining element in
          -- visit and any nodes in the neighbourhood not already visited
          -- note that this removes next from visit' also as we
          -- did put it into visited' above
          visit' = Set.difference (Set.union visit ns) visited'
        in
        -- recursively move on
        go visited' visit'
      -- seems there is no node to visit so
      -- the recursion ends here with all nodes
      -- visited
      Nothing -> visited