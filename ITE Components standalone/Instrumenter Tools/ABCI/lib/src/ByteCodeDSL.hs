{-# LANGUAGE GADTs, KindSignatures, Arrows, BangPatterns #-}
module ByteCodeDSL
  ( InjA, InjFun, prepareEval
  , uid, debugMsg, store, fetch, instr, abort, ctxGet, ctxGets, ctxGets', memo, unsafeMemo
  , ctxCurrentTbls, ctxCurrentBlockId, ctxCurrentMethodId, ctxCurrentParents, getOptions
  ) where


import Control.Arrow
import Control.Arrow.Transformer.All
import Control.Category hiding (id, (.))
import qualified Control.Category as C
import Data.Word
import Data.Monoid
import ByteCode
import ByteCodeTrf
import ProgInfo
import Data.Set(Set)
import Data.Map(Map)
import Data.IntMap(IntMap)
import Data.IntSet(IntSet)
import Data.Sequence(Seq)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Debug.Trace
import Control.Monad.RWS.Strict
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Unsafe.Coerce
import Heap
import Options


--
-- Interface to evaluator
--

prepareEval :: InjA () a -> Int -> (InjFun, Int)
prepareEval !a !g = (f, g') where
  !(!c, !g') = toCoreMemo a g
  !m         = compileCore c
  !f         = execInjM m


--
-- Instrumentation Language
-- Arrow-based front-end
--
-- It annotates the arrow core language with unique
-- numbers, and a static analysis result to find out
-- if iteration with respect to the control-flow graph.
--

newtype InjA a b = InjA (Int -> ( InjC a b  -- the annotated arrow in the core language
                                , Int       -- updated unique counter
                                , Bool      -- True if contains heap loads and stores
                                ))

-- Composition combinators
instance Category InjA where
  id        = noneInjA InjId
  (.) !p !q = binInjA InjComp p q

instance Arrow InjA where
  arr !f   = noneInjA (InjArr f)
  first !p = sinInjA InjFirst p

instance ArrowZero InjA where
  zeroArrow = noneInjA (InjFail "zero")

instance ArrowPlus InjA where
  (<+>) !p !q = binInjA InjAlt p q

instance ArrowChoice InjA where
  left !p = sinInjA InjLeft p

-- The ArrowApply instance should be used with caution.
-- There should not be any "declare"-like operations
-- in the right-hand side. Enforced with a runtime check.
-- Todo: use the type system to enforce this statically.
instance ArrowApply InjA where
  app = unqInjA InjApply

instance ArrowLoop InjA where
  loop = sinInjA InjFix

-- Support primitives
uid :: InjA () Int
uid = unqInjA InjUid

debugMsg :: InjA String ()
debugMsg = noneInjA InjDebug

ctxGet :: InjA () CtxInfo
ctxGet = noneInjA InjAsk

store :: InjA (Key a, Threading, a) ()
store = noneInjA InjStore

fetch :: InjA (Key a) a
fetch = noneInjA InjFetch

unsafeMemo :: InjA a b -> InjA a b
unsafeMemo = unqSinInjA InjMemo

memo :: InjA () b -> InjA () b
memo = unqSinInjA InjMemo

-- Actual primitives
instr :: InjA LabInstruction ()
instr = noneInjA InjInstr

-- General unique number threading
noneInjA :: InjC a b -> InjA a b
noneInjA !c = InjA (\(!x) -> (c, x, eff)) where
  !eff = case c of
    InjFetch -> True
    InjStore -> True
    _        -> False

unqInjA :: (Int -> InjC a b) -> InjA a b
unqInjA f = InjA (\(!x) ->
  let !r  = f x
      !x' = x + 1
  in (r, x', False))

sinInjA :: (InjC a b -> InjC c d) -> InjA a b -> InjA c d
sinInjA !c !(InjA !f) = InjA (\(!x0) ->
  let !(!af, !x1, !eff) = f x0
      !r = c af
  in (r, x1, eff))

unqSinInjA :: (Int -> InjC a b -> InjC c d) -> InjA a b -> InjA c d
unqSinInjA !c !(InjA !f) = InjA (\(!x0) ->
  let !(!af, !x1, !eff) = f x0
      !r  = c x1 af
      !x2 = x1 + 1
  in (r, x2, eff))

binInjA :: (InjC a b -> InjC c d -> InjC e f) -> InjA a b -> InjA c d -> InjA e f
binInjA !c !(InjA !f) !(InjA !g) = InjA (\x0 ->
  let !(!af, !x1, !eff1) = f x0
      !(!ag, !x2, !eff2) = g x1
      !eff = eff1 || eff2
  in (c af ag, x2, eff))

-- converts the arrow into the core arrow, which annotates it with unique numbers
toCore :: InjA a b -> Int -> (InjC a b, Int, Bool)
toCore !(InjA !f) !gIn = f gIn where

-- Requires an arrow with constant input, which is the case for the
-- root of the instrumentation. If the arrow does not use fetch/store
-- of heap values (and thus does not require fixpoint iteration)
-- we memo its results, which will then only run it once
toCoreMemo :: InjA () b -> Int -> (InjC () b, Int)
toCoreMemo !(InjA !f) !gIn
  | eff       = (p, g)  -- it uses fetch store, therefore cannot memo it
  | otherwise = let !g' = g + 1
                    !q  = InjMemo g p
                in (q, g')
  where !(!p, !g, !eff) = f gIn


--
-- Core instrumentation language
-- Arrow-based core
--

data InjC :: * -> * -> * where
    -- Composition of arrows
  InjId     :: InjC a a
  InjArr    :: !(a -> b) -> InjC a b
  InjComp   :: !(InjC b c) -> !(InjC a b) -> InjC a c
  InjFirst  :: !(InjC b c) -> InjC (b, d) (c, d)
  InjLeft   :: !(InjC b c) -> InjC (Either b d) (Either c d)
  InjApply  :: !Int -> InjC (InjA b c, b) c
  InjFix    :: InjC (b,d) (c,d) -> InjC b c
  InjMemo   :: !Int -> !(InjC b c) -> InjC b c

    -- Failure
  InjFail   :: !String -> InjC a b
  InjAlt    :: !(InjC a b) -> !(InjC a b) -> InjC a b

    -- Support operations
  InjUid    :: !Int -> InjC () Int
  InjDebug  :: InjC String ()
  InjAsk    :: InjC () CtxInfo
  InjFetch  :: InjC (Key a) a
  InjStore  :: InjC (Key a, Threading, a) ()

    -- Actual operations
  InjInstr  :: InjC LabInstruction ()



--
-- Evaluator for the core language
--
-- Note about the monad composition:
-- the error monad is used to encode guards and conditionals.
-- A failing thread of execution may have effects on the state,
-- and code up-till the point of failure is generated.
--

type InjM a = ErrorT String (RWS CtxInfo InjExpr StateInfo) a

data StateInfo = StateInfo
  { stateThrHeap  :: !Heap      -- the threaded heap
  , statePrevHeap :: !Heap      -- updates for predecessors
  , stateSuccHeap :: !Heap      -- updates for successors
  , stateUid      :: !Int       -- unique number updates
  , stateMemo     :: !MemoInjs  -- memo'd injections
  }

execInjM :: InjM a -> InjFun
execInjM !m !ctxInfo !heap (InjShared uidIn memosIn) = out where
  !inh      = ctxInfo
  !chn      = StateInfo heap mempty mempty uidIn memosIn
  !(!s, !w) = execRWS (runErrorT m) inh chn
  !out      = (stateThrHeap s, statePrevHeap s, stateSuccHeap s, w, InjShared (stateUid s) (stateMemo s))

-- Prepare sequential pass through the arrow
compileCore :: InjC () b -> InjM b
compileCore !a = runCore a ()

-- Backtracking interpretation of the arrow with the InjM monad
runCore :: InjC a b -> a -> InjM b
runCore !ar !inp = case ar of
  InjId        -> return inp
  InjArr f     -> return $! f inp
  InjComp g f  -> do
    !fOut <- runCore f inp
    !gOut <- runCore g fOut
    return gOut
  InjFirst f   -> case inp of
    (!a, !b)   -> do
      !fOut <- runCore f a
      return (fOut, b)
  InjLeft f    -> case inp of
    Left !b    -> do
      !fOut <- runCore f b
      return $! Left fOut
    Right !d   -> return $! Right d
  InjApply u   -> fromMemo u $ case inp of
      (!c, !b) -> do
        !uIn <- gets stateUid
        let !(!p, !uOut, !usesEff) = toCore c uIn
        modify (\s -> s { stateUid = uOut })
        !(a, t) <- listen (runCore p b)
        when (uOut /= uIn) $
          if usesEff
          then let msg = "InjApply: cannot memo monadic instrumentation that depends on load/store in heap"
               in trace msg $ throwError msg
          else storeMemo u a t
        tell t
        return a
  InjFix p     -> mfix (\out -> runCore p (inp, snd out)) >>= return . fst
  InjFail s    -> throwError s
  InjAlt p q   -> runCore p inp `catchError` \_ -> runCore q inp
  InjUid u     -> return u
  InjMemo u p  -> fromMemo u $ do
    !(a, t) <- listen (runCore p inp)
    storeMemo u a t
    tell t
    return a
  InjFetch     -> do
    !hp <- gets stateThrHeap
    case fetchFromHeap inp hp of
      Nothing  -> throwError ("Not in heap: " ++ show inp)
      Just !v  -> return v
  InjStore     -> case inp of
    (!k,!t,!v) -> do
      let f st = st { stateThrHeap  = storeInHeap k v (stateThrHeap st)
                    , statePrevHeap = prv (statePrevHeap st)
                    , stateSuccHeap = nxt (stateSuccHeap st) } where
            prv | t == ThreadPrev || t == ThreadGlobal = storeInHeap k v
                | otherwise                            = id
            nxt | t == ThreadSucc || t == ThreadGlobal = storeInHeap k v
                | otherwise                            = id
      modify f
  InjDebug    -> trace inp (return ())
  InjAsk      -> ask
  InjInstr    -> tell $! InjExpr_PushInstr $ const inp

storeMemo :: Int -> a -> InjExpr -> InjM ()
storeMemo !u !a !t =
  let !v = MemoInj t a
  in modify (\s -> s { stateMemo = IntMap.insert u v (stateMemo s) })

fromMemo :: Int -> InjM a -> InjM a
fromMemo u m = do
  mem <- gets stateMemo
  case IntMap.lookup u mem of
    Just (MemoInj t a) -> do
      tell t
      return (unsafeCoerce a)  -- note: memo keys have to be unique for this to succeed
    Nothing -> m


--
-- Derived utility combinators
--

ctxGets :: InjA (CtxInfo -> a) a
ctxGets = proc f -> do
            !ctx <- ctxGet -< ()
            returnA -< (f ctx)

ctxGets' :: (CtxInfo -> a) -> InjA () a
ctxGets' !f = ctxGet >>> arr f

abort :: InjA a b
abort = zeroArrow


-- Querying
ctxCurrentTbls :: InjA () SymbolTables
ctxCurrentTbls = ctxGets' ctxTbls

ctxCurrentBlockId  :: InjA () Int
ctxCurrentBlockId = ctxGets' (ctxBlockId . ctxBlock)

ctxCurrentMethodId :: InjA () Word32
ctxCurrentMethodId = ctxGets' (ctxMethodId . ctxMethod)

ctxCurrentParents :: InjA () CtxParents
ctxCurrentParents = ctxGets' ctxParents

getOptions :: InjA () Options
getOptions = ctxGets' ctxOpts


-- Embed Haskell constants in the code
{-
instance Embeddable a where
  embed :: a -> Code () (PrimSym a)

instance Embeddable Int    where embed = declInt
instance Embeddable Word32 where embed = declUInt
instance Embeddable Double where embed = declDouble
instance Embeddable String where embed = declString
instance Embeddable
-}
