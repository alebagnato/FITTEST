{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances, BangPatterns #-}
module Fixpoint(NodeFun,FixGraph,FixSem,NodeId,NodeVal,EdgeVal,edge,node,transpose,solve,solve',NodeFunIn(..),NodeFunOut(..),funOut, SolveState) where

import Data.Maybe
import Data.Monoid
import Data.Map(Map)
import Data.STRef
import qualified Data.Map as Map
import Control.Monad.ST
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import Data.Array.ST
import qualified Data.Array as A
import Control.Monad


-- Types for values computed for edges and nodes
-- 'a' is a type index.
type family EdgeVal a    -- type of values computed for an edge
type family NodeVal a    -- type of values computed for a node
type family NodeId a     -- identification of a node
type family SolveState a -- shared state during solving

--
-- Note: the graph itself is a directed graph with unlabeled edges.
--       the nodes of the graph are labeled with a 'NodeId a'.
--       the semantics for each node is determined by a map from
--       'NodeId a' to 'NodeFun a'.
--       'solve' on the graph returns a map from 'NodeId a' to 'NodeVal a'.
--

-- For a node, we may specify a NodeFun, which specifies how to compute the EdgeVal
-- for all outgoing edges based on the input EdgeVals. Additionally, the
-- function gets a possible previous NodeVal and EdgeVal result as additional
-- input. If the result did not change, the function should return Nothing
-- as EdgeVal result.
--
-- Its input is a list of the EdgeVals of the predecessors that produced an
-- output so far.
type NodeFun a = NodeFunIn a -> NodeFunOut a
data NodeFunIn a = NodeFunIn { nfInputs :: ![EdgeVal a], nfNodeState :: !(Maybe (NodeVal a)), nfPrevRes :: !(Maybe (EdgeVal a)), nfSharedIn :: !(SolveState a) }
data NodeFunOut a = NodeFunOut { nfNewState :: !(Maybe (NodeVal a)), nfOutput :: !(Maybe (EdgeVal a)), nfSharedOut :: !(SolveState a) }

-- Default value for output
funOut :: SolveState a -> NodeFunOut a
funOut st = NodeFunOut { nfNewState = Nothing, nfOutput = Nothing, nfSharedOut = st }

emptyNodeFun :: NodeFunIn a -> NodeFunOut a
emptyNodeFun (NodeFunIn _ _ _ st) = funOut st

-- | The fix graph is a sparse, directed graph.
newtype FixGraph a = FixGraph (Set (NodeId a, NodeId a))
newtype FixSem a = FixSem (Map (NodeId a) (NodeFun a))

-- | Monoid instance that joins fixgraphs.
instance Ord (NodeId a) => Monoid (FixGraph a) where
  mempty = FixGraph Set.empty
  (FixGraph !p) `mappend` (FixGraph !q) = FixGraph $! (Set.union p q)

instance Ord (NodeId a) => Monoid (FixSem a) where
  mempty = FixSem Map.empty
  (FixSem !p) `mappend` (FixSem !q) = FixSem $! (Map.union p q)

instance Show (NodeId a) => Show (FixGraph a) where
  show (FixGraph !s) = unwords [ show a ++ "->" ++ show b | (a,b) <- Set.toList s ]

-- | Turns a single edge into a fixgraph.
edge :: Ord (NodeId a) => NodeId a -> NodeId a -> FixGraph a
edge !a !b = FixGraph $! Set.singleton $! (a,b)

-- | Turns a node semantics in a FixSem.
node :: Ord (NodeId a) => NodeId a -> NodeFun a -> FixSem a
node !n !v = FixSem $! Map.singleton n v

transpose :: Ord (NodeId a) => FixGraph a -> FixGraph a
transpose (FixGraph !s) = FixGraph $! Set.map (uncurry $ flip $ (,)) s

-- | Runs a fixpoint computation over the graph using a worklist algorithm.
--   Specify the graph, the semantics of the nodes, and a map with initial values for
--   the *edges* that leave the nodes as mention in the map.
--   The result is a map with the final values for each node.
solve :: (Ord (NodeId a), Show (NodeId a)) => FixGraph a -> FixSem a -> Map (NodeId a) (EdgeVal a) -> Map (NodeId a) (NodeVal a) -> SolveState a -> (Map (NodeId a) (NodeVal a), SolveState a)
solve (FixGraph !g) (FixSem !sems) !edgeInitial ndInitial !st0 = runST $ do
  !edgeVals  <- newSTArray (1, size) Nothing -- values of all nodes' outgoing edges
  !nodeVals  <- newSTArray (1, size) Nothing -- values of all nodes' state
  !pending   <- newSTUArray (1, size) False  -- index of nodes in the pending list
  !sharedRef <- newSTRef st0

  -- store initial values in the node and edge arrays
  mapM_ (\(k,v) -> writeArray edgeVals (Map.findWithDefault (error ("solve:3:not in map:" ++ show k)) k fwdMap) (Just v)) (Map.assocs edgeInitial)
  mapM_ (\(k,v) -> writeArray nodeVals (Map.findWithDefault (error ("solve:3:not in map:" ++ show k)) k fwdMap) (Just v)) (Map.assocs ndInitial)

  let !init = [1 .. size]

      worklist []     = return ()
      worklist (x:xs) = do
        writeArray pending x False

        -- determine successors and predecessors
        let !preds = predsA A.! x
            !succs = succsA A.! x
        !mbInps    <- mapM (readArray edgeVals) preds
        !mbNodeVal <- readArray nodeVals x
        !mbPrevVal <- readArray edgeVals x
        !stIn      <- readSTRef sharedRef

        -- apply function
        let !semf = semsA A.! x
            !inps = catMaybes mbInps
            !(NodeFunOut !mbUpdate !mbNextVal !stOut) = semf (NodeFunIn inps mbNodeVal mbPrevVal stIn)

        -- update the shared state
        writeSTRef sharedRef stOut

        -- update prev value
        case mbUpdate of
          Nothing -> return ()
          Just _  -> writeArray nodeVals x mbUpdate

        -- determine nexts
        !more <- case mbNextVal of
                   Nothing -> return []
                   Just _  -> do
                     writeArray edgeVals x mbNextVal
                     filterM (fmap not . readArray pending) succs

        worklist (more ++ xs)

  -- run the worklist algorithm
  worklist init

  -- read the items from the vals array and translate them back to the node-domain
  let fetch k = readArray nodeVals (Map.findWithDefault (error "solve:5:not in map") k fwdMap)
  !sol <- (Map.fromList . catMaybes) `fmap` mapM (\k -> fetch k >>= return . fmap (\e -> (k,e))) nodes
  !st' <- readSTRef sharedRef
  return (sol, st')
  where
    !size   = length nodes
    !nodesFromEdges    = Set.fromList [ x | (a, b) <- Set.toAscList g, x <- [a,b] ]
    !nodesFromEInitial = Map.keysSet edgeInitial
    !nodesFromNInitial = Map.keysSet ndInitial
    !nodesFromSems     = Map.keysSet sems
    !nodes  = Set.toAscList (nodesFromSems `Set.union` nodesFromEdges `Set.union` nodesFromEInitial `Set.union` nodesFromNInitial)
    !fwdMap = Map.fromList $ zip nodes [1..]

    !succMap = Map.fromListWith (++) [ (a,[b]) | (a,b) <- Set.toAscList g ]
    !predMap = Map.fromListWith (++) [ (b,[a]) | (a,b) <- Set.toAscList g ]
    !succsA  = A.array (1, size) [ (i, repEdges succMap n) | (n,i) <- Map.assocs fwdMap ]
    !predsA  = A.array (1, size) [ (i, repEdges predMap n) | (n,i) <- Map.assocs fwdMap ]
    !semsA   = A.array (1, size) [ (i,  Map.findWithDefault emptyNodeFun n sems) | (n,i) <- Map.assocs fwdMap ]

    repEdges mp n = [ Map.findWithDefault (error "solve:2:not in map") v fwdMap
                    | v <- Map.findWithDefault [] n mp ]

-- Variant that does not use the shared state
solve' :: (Ord (NodeId a), Show (NodeId a), SolveState a ~ ()) => FixGraph a -> FixSem a -> Map (NodeId a) (EdgeVal a) -> Map (NodeId a) (NodeVal a) -> Map (NodeId a) (NodeVal a)
solve' g s e0 n0 = fst $ solve g s e0 n0 ()

-- | Makes the type of the array concrete such that we can use the MArray operations in the ST monad.
newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
newSTArray = newArray

newSTUArray :: (Ix i, MArray (STUArray s) e (ST s)) => (i,i) -> e -> ST s (STUArray s i e)
newSTUArray = newArray
