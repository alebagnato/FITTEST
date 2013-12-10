-- | Typed Embedded Instrumentation Language for Object-Oriented programming languages.
-- Haskell provides abstraction facilities and meta-programming.
--
-- The language specifies conditional instrumentation to be performed at
-- program points. See 'LanguageAst.ag' for a description of the underlying
-- core language.
--
-- Note the following phase destinction: execution of the Haskell program produces
-- a description for how to perform the instrumentation. This is a preprocessing step.
-- Interpretation of this description is what we call "compile time". Execution of the
-- instrumented program is what we call "runtime".
--
-- The typed instrumentation language is a monad. The computation
-- itself consists of two essential concepts: matches against program
-- points and insertion of function calls. Program points include method calls and
-- entry/exists of a sequential block of instructions. Each instrumentation can succeed
-- or fail. When the instrumentation succeeded, the value asociated to the monad captures
-- the context information, e.g. binding names to parameters of a function call.
-- Instrumentations can be composed sequentially (m >>= f): the right-hand side |f| is
-- parametrized with the context of the left-hand side |m|, and executed, iff |m| succeeds.
-- The instrumentation can be partially evaluated: matches based on statically known
-- information can be resolved at instrumentation time. Most function calls are required to
-- be done at runtime due to side effect.
--
-- For example, suppose that we want to instrument each
-- entry of a block of instructions with a call to a provided
-- function |enterCycBlock(id)|, where |id| is a unique identification
-- of the block. Moreover, also suppose that we are only interested
-- in intrumenting blocks on a cyclic path in the control flow graph.
-- For that, we write the following monadic expression (explained below):
--
-- > do info <- matchEnterBlock
-- >    equals (isCycl info) (con True)
-- >    call fenterBlock (blockId info)
--
-- The context-information after matching against the block is bound to
-- the Haskell value |info|. This value contains embedded instrumentation
-- values |isCycl info| and |blockId info|. The first is a boolean value
-- that is |True| iff the block is on a cyclic CFG path. The second is an
-- integer identifying the block.
-- 
-- Both are embedded values: values of the type |Var Bool| and |Var Int| respectively.
-- Also, the values are statically known: a corresponding Haskell value
-- can be deduced when the program is instrumented. Some values, e.g. those
-- bound to the inputs of a method for a match against a method call, are
-- dynamic values. These have a symbolic value at instrumentation time.
--
-- The |equals| invocation tests if its two input embedded values are equal.
-- When both values are statically known, the test is resolved at instrumentation time,
-- otherwise it leaves a residual test that succeeds or fails at runtime.
-- The Haskell function |con| encodes a Haskell value as embedded constant,
-- which is known at instrumentation time. Hence, both inputs are known at
-- instrumentation time, and the check can be resolved statically. As a
-- consequence, only residual code is generated for the call to "enterBlock", for
-- only the intended program points. For all other program points, the
-- instrumentation fails already at instrumentation time.
--
-- A failing instrumentation is not an error: it just means that the
-- instrumentation is only performed up to the point of failure for a
-- program point. Care must be taken when side-effect is involved.

{-# LANGUAGE EmptyDataDecls, GADTs, KindSignatures, ScopedTypeVariables,
    MultiParamTypeClasses, FunctionalDependencies, TypeFamilies,
    FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeSynonymInstances #-}
module Instr where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Word
import qualified Language as L


--
-- Variables and Values
-- Certain Haskell values can (easily) be promoted as embedded constants. (Symbolic) values
-- are obtained via intrumentation instructions, and cannot be constructed directly.
-- Embedded constant values can be mapped back to their Haskell values.
--

-- | A variable is a symbolic value of a certain type.
--   It is associated with an integer value that can be used to identify the
--   symbolic value. There may be a statically known substitution for a
--   symbolic value.
data Var :: * -> * where
  Sym :: Int -> Var a

-- | A constant is a fixed value of a certain type.
data Con :: * -> * where
  ConInt     :: Int     -> Con Int
  ConUInt    :: Word32  -> Con Word32
  ConBool    :: Bool    -> Con Bool
  ConString  :: String  -> Con String
  ConArray   :: [Val a] -> Con [a]
  ConType    :: Type a  -> Con (Type a)
  ConNothing ::            Con (Maybe a)
  ConJust    :: Val a   -> Con (Maybe a)
  ConMethod  :: String  -> FunSpec a b -> Con (FunSpec a b)

-- | Values
data Val  :: * -> * where
  ValSym    :: Var a -> Val a
  ValCon    :: Con a -> Val a
  ValInd    :: Val [a] -> Val Word32 -> Val a
  ValProp   :: IsSuper c a => Val a -> Key c b -> Val b
  ValStatic :: Key c b -> Val b  -- not checked that b is a static field
  ValDyn    :: IsSuper a Base => Val a -> Val String -> Val Any

-- | Returns the Haskell value associated to a constant embedded value.
--   Those parts of the value that are not constant, are mapped to bottom.
unCon :: Val a -> a
unCon (ValCon c)   = unCon' c
unCon (ValInd a i) = unCon a !! (fromIntegral $ unCon i)
unCon _ = error "unCon on a non-constant value"

-- | Returns the wrapped Haskell value.
unCon' :: Con a -> a
unCon' (ConInt i)      = i
unCon' (ConUInt i)     = i
unCon' (ConBool b)     = b
unCon' (ConString s)   = s
unCon' (ConArray xs)   = map unCon xs
unCon' (ConType t)     = t
unCon' (ConNothing)    = Nothing
unCon' (ConJust x)     = Just (error "unCon'::value")
unCon' (ConMethod x s) = s

-- | Statically known index in an object.
data Key :: * -> * -> * where
  Key :: String -> Type b -> Key a b

-- | Embedding of constants.
-- Prefixing a Haskell value with |con|, turns the Haskell value
-- into an embedded constant.
class IsConstant a where
  con :: a -> Val a

instance IsConstant String where
  con = ValCon . ConString

instance IsConstant Bool where
  con = ValCon . ConBool

instance IsConstant Int where
  con = ValCon . ConInt

instance IsConstant Word32 where
  con = ValCon . ConUInt

instance IsConstant (Type a) where
  con = ValCon . ConType


-- | Specification of embedded types.
--   The idea is to have for every type occurring in a program (embedded types),
--   a Haskell value that describes this type, and a constructor-less corresponding
--   Haskell type for object/interface types.
--   The Haskell value can be turned into an embedded constant describing
--   this type.
--   The embedded types/values are transcribed to Haskell names: the types are
--   prefixed with a capital 'T', the values with a small 't'.

-- | Description of a type
data Type :: * -> * where
  TypeAny     :: Type Any
  TypeBool    :: Type Bool
  TypeInt     :: Type Int
  TypeUInt    :: Type Word32
  TypeDouble  :: Type Double
  TypeString  :: Type String
  TypeArray   :: Type a -> Type [a]   -- Allows Type-parametrized arrays to be specified
  TypeBase    :: Type Base
  TypeObject  :: String -> Type a
  TypeMethod  :: FunSpec a b -> Type (FunSpec a b)

instance Show (Type a) where
  show t = case t of
    TypeAny      -> "T˚Any"
    TypeBool     -> "T˚Bool"
    TypeInt      -> "T˚Int"
    TypeUInt     -> "T˚UInt"
    TypeDouble   -> "T˚Double"
    TypeString   -> "T˚String"
    TypeArray t  -> "T˚Array˚" ++ show t
    TypeBase     -> "T˚Base"
    TypeObject s -> "T˚" ++ s
    TypeMethod _ -> "T˚Method"

-- | The untyped object.
data Any :: *
tAny :: Type Any
tAny = TypeAny

-- | The base type of all types.
data Base :: *
tBase :: Type Base
tBase = TypeBase


-- | Specification of coercions.
--   With coercions, we explicitly encode the conversion of certain types ot other types.
--   In particular, we do not support automatic upcasting of types: this can be done
--   via |CoerceInstance|.
--   To cast (possibly fails) a value of some arbitrary type to another type, use a
--   coercion from and to |Any|.
data Coercion :: * -> * -> * where
  CoerceNone      :: Coercion a a     -- explicit absence of coercion
  CoerceAny       :: Coercion a Any   -- arbitrary coercion
  CoerceAny'      :: Coercion Any b
  CoerceString    :: Coercion a String
  CoerceDouble    :: Coercion a Double
  CoerceInt       :: Coercion a Int
  CoerceUInt      :: Coercion a Word32
  CoerceInstance  :: IsSuper a b => Coercion a b

-- | Matches an arbitrary coercion/represents an arbitrary coercion.
coeAny :: Coercion Any Any
coeAny = CoerceAny

-- | Describes the inheritance relation between classes/interfaces.
-- This is a non-symmetric, transitive and reflexive relation.
-- When a type extends or implements another type, we capture this
-- as an instance of |IsSuper|. Note that the instances may be
-- overlapping: that doesn't matter. As the class has no members,
-- we are only interested in the existence of membership of the
-- relation, and not in particular memberships.
-- Is |a| a superclass of |b|?
class IsSuper a b -- captures inheritance relation

-- | Automatically constructs coercions related to upcasting.
--   Also, an untyped value (of type |Any|) can always be
--   automatically downcast to any other value.
class HasCoercion a b where
  mkCoercion :: Coercion a b

instance HasCoercion a a where
  mkCoercion = CoerceNone

instance IsSuper a b => HasCoercion a b where
  mkCoercion = CoerceInstance

instance HasCoercion Any b where
  mkCoercion = CoerceAny'


--
-- Typed expressions
-- An |Expr a| is mappeable to a |Val a|.
-- Expressions may contain nested instrumentations
--

-- Expressions
data Expr :: * -> * where
  ExprVal    :: Val a -> Expr a
  ExprUn     :: UnOp a b -> Expr a -> Expr b
  ExprBin    :: BinOp a b c -> Expr a -> Expr b -> Expr c
  ExprInstr  :: Instr (Val a) -> Expr a     -- nested instrumentation (with a 'Val' as return value)

-- Binary operators
data BinOp :: * -> * -> * -> * where
  OpAnd :: BinOp Bool Bool Bool
  OpOr  :: BinOp Bool Bool Bool
  OpRel :: BinRel a -> BinOp a a Bool
  OpAdd :: IsNumeric a => BinOp a a a
  OpSub :: IsNumeric a => BinOp a a a
  OpMul :: IsNumeric a => BinOp a a a
  OpDiv :: IsNumeric a => BinOp a a a
  OpMod :: IsNumInt a  => BinOp a a a
  OpMax :: IsNumeric a => BinOp a a a
  OpMin :: IsNumeric a => BinOp a a a

-- Unary operators
data UnOp :: * -> * -> * where
  OpAbs       :: IsNumeric a => UnOp a a
  OpNeg       :: IsNumeric a => UnOp a a
  OpNot       :: UnOp Bool Bool
  OpIsJust    :: UnOp (Maybe a) Bool
  OpIsNothing :: UnOp (Maybe a) Bool
  OpExtract   :: UnOp (Maybe a) a
  OpLength    :: UnOp [a] Int

-- Binary relations
data BinRel :: * -> * where
  RelEqual        :: BinRel a
  RelSmaller      :: IsComparable a => BinRel a
  RelSmallerEqual :: IsComparable a => BinRel a
  RelGreater      :: IsComparable a => BinRel a
  RelGreaterEqual :: IsComparable a => BinRel a

class IsComparable a
instance IsComparable Bool
instance IsNumeric a => IsComparable a

class IsNumeric a
instance IsNumInt a => IsNumeric a
instance IsNumeric Double

class IsNumInt a
instance IsNumInt Int
instance IsNumInt Word32

-- | Conversion of different forms of values to expressions.
class IsExpr a b | a -> b where
  toExpr :: a -> Expr b

instance IsExpr (Expr a) a where
  toExpr = id

instance IsExpr (Val a) a where
  toExpr = ExprVal

instance IsExpr Int Int where
  toExpr = ExprVal . con

instance IsExpr Bool Bool where
  toExpr = ExprVal . con

instance IsExpr Word32 Word32 where
  toExpr = ExprVal . con

{-
-- | Data-types used to distinguish dictionaries of 'IsExpr'
data DictPrim  = DictPrim
data DictConst = DictConst
data DictCoe   = DictCoe

class ExprDict d where exprDict :: d
instance ExprDict DictPrim  where exprDict = DictPrim
instance ExprDict DictConst where exprDict = DictConst
instance ExprDict DictCoe   where exprDict = DictCoe

class IsExpr' d a b | a -> d where
  toExpr' :: d -> a -> Expr b

instance (ExprDict d, IsExpr' d a b) => IsExpr a b where
  toExpr = toExpr' (exprDict :: d)

instance (d ~ DictPrim) => IsExpr' d (Val a) a where
  toExpr' DictPrim = ExprVal

instance (d ~ DictPrim) => IsExpr' d (Var a) a where
  toExpr' DictPrim = toExpr . ValSym

instance (d ~ DictPrim) => IsExpr' d (Expr a) a where
  toExpr' DictPrim = id

instance (d ~ DictConst, IsConstant a) => IsExpr' d a a where
  toExpr' DictConst = toExpr . con
-}


--
-- Declaration of a function call
--

-- | Specification of a function, which gives the
--   types of the parameters and the types of the
--   return values. Typically, functions have only
--   zero or one return values.
data FunSpec :: * -> * -> * where
  FunSig :: ParamSpecs a -> ParamSpecs b -> FunSpec a b

-- Arbitrary function.
anyFun :: FunSpec Any Any
anyFun = FunSig SpecsAny SpecsAny

-- | Function without a return value.
anyFun0 :: FunSpec Any ()
anyFun0 = FunSig SpecsAny SpecsEmpty

-- | Specifies the types of the parameters.
data ParamSpecs :: * -> * where
  SpecsAny   :: ParamSpecs Any                     -- any parameter specification
  SpecsEmpty :: ParamSpecs ()
  SpecsCons  :: Type a -> ParamSpecs b -> ParamSpecs (a, b)

-- A typed product of parameter values.
data Params :: * -> * where
  ParamsAny   :: Val [Any] -> Params Any           -- array of additional paramsters (of unknown type)
  ParamsEmpty :: Params ()
  ParamsCons  :: Val a -> Params b -> Params (a, b)

-- A Typed product of expression values.
data Exprs :: * -> * where
  ExprsAny   :: Expr [Any] -> Exprs Any            -- array of expressions of unknown type
  ExprsEmpty :: Exprs ()                           -- no expressions
  ExprsCons  :: Expr a -> Exprs b -> Exprs (a, b)  -- combination of expressions

class IsExprs a p | a -> p where   -- syntactic sugar to translate values into Exprs values.
  toExprs :: p -> Exprs a

-- Convenience function to build 'Exprs' values.
instance IsExprs Any (Expr [Any])        where  toExprs    = ExprsAny                         -- arbitrary parameters
instance IsExprs () ()                   where  toExprs () = ExprsEmpty                       -- zero parameters
instance IsExpr p a => IsExprs (a, ()) p where  toExprs e  = ExprsCons (toExpr e) ExprsEmpty  -- one parameter
instance (IsExpr p a, IsExpr q b) => IsExprs (a, (b, ())) (p, q)
  where  toExprs (e1,e2) = ExprsCons (toExpr e1) $ toExprs e2                                 -- two parameters
instance (IsExpr p a, IsExpr q b, IsExpr r c) => IsExprs (a, (b, (c, ()))) (p, q, r)
  where  toExprs (e1,e2,e3) = ExprsCons (toExpr e1) $ toExprs (e2,e3)                         -- three parameters
instance (IsExpr p a, IsExpr q b, IsExpr r c, IsExpr s d) => IsExprs (a, (b, (c, (d, ())))) (p, q, r, s)
  where  toExprs (e1,e2,e3,e4) = ExprsCons (toExpr e1) $ toExprs (e2,e3,e4)                   -- four parameters
instance (IsExpr p a, IsExpr q b, IsExpr r c, IsExpr s d, IsExpr t e) => IsExprs (a, (b, (c, (d, (e, ()))))) (p, q, r, s, t)
  where  toExprs (e1,e2,e3,e4,e5) = ExprsCons (toExpr e1) $ toExprs (e2,e3,e4,e5)             -- five parameters

-- | Overloaded to extract several forms of parameter specifications of functions.
class InputSpecs a b | a -> b where
  inputSpecs :: a -> ParamSpecs b

-- | Overloaded to extract server forms of return-type specifications of functions.
class OutputSpecs a b | a -> b where
  outputSpecs :: a -> ParamSpecs b

-- | Extracts the specifications.
instance InputSpecs (FunSpec a b) a          where  inputSpecs (FunSig p _) = p
instance InputSpecs (ParamSpecs a) a         where  inputSpecs = id
instance InputSpecs (Key c (FunSpec a b)) a  where  inputSpecs = inputSpecs  . mkFunSpec

instance OutputSpecs(FunSpec a b) b           where  outputSpecs (FunSig _ p) = p
instance OutputSpecs (ParamSpecs a) a         where  outputSpecs = id
instance OutputSpecs (Key c (FunSpec a b)) b  where  outputSpecs = outputSpecs . mkFunSpec

mkFunSpec :: Key c (FunSpec a b) -> FunSpec a b
mkFunSpec (Key _ (TypeMethod spec)) = spec
mkFunSpec _ = error "mkFunSpec: key has function type, but is not of the method type"


--
-- Specification of a match on a join point
--

-- | Generic joinpoint information
data MatchInfo :: * -> * where
  MatchInfo :: { matchId :: Val Int, matchInfo :: a } -> MatchInfo a

-- | The result of matching against a coercion.
data CoerceMatch :: * -> * -> * where
  CoerceMatch :: { coerceCoe :: Coercion a b, coerceInfo :: CoerceInfo a b } -> CoerceMatch a b

-- | Information about what sort of coercion match we are talking about
data CoerceInfo :: * -> * -> * where
  CoerceEntry :: { coerceInp :: Val a } -> CoerceInfo a b
  CoerceDone  :: { coerceOut :: Val a } -> CoerceInfo a b
  CoerceFail  :: { coerceInp :: Val a, coerceExcp :: Val Any } -> CoerceInfo a b

-- | The result of a match against a block.
-- It consists of a unique identifier for the block, and
-- a Boolean value that identifies if the block is cyclic or not,
-- and a Maybe value that indicates if the block exited with an exception, or is an exception handler.
data BlockMatch :: * where
  BlockMatch :: { blockCycl :: Val Bool, blockExcp :: Val (Maybe Any) } -> BlockMatch

-- | The result of a match against a function.
-- It consists of the name of the function, the kind of the
-- function (i.e. property or method), and either the input
-- or output values.
-- May match against a failing function.
data FunMatch :: * -> * where
  FunMatch :: { funName :: Val String, funKind :: Val FunKind, funInfo :: a } -> FunMatch a

-- | Match info for a function that is called or has succeeded
data FunParams :: * -> * where
  FunParams :: { funParams :: Params a  } -> FunParams a

-- | Match info for a failed function.
data FunFail :: * where
  FunFail :: { funExcp :: Val Any } -> FunFail

-- | The kind of a function. Either a method or property (that get the object
-- as first parameter), or an objectless function.
data FunKind :: * where
  KindFunction :: FunKind
  KindMethod   :: FunKind
  KindProperty :: FunKind

-- | Returns the number of values in the parameter product as Haskell value.
numParams :: Params a -> Int
numParams ParamsEmpty       = 0
numParams (ParamsCons _ ps) = 1 + numParams ps

-- | Get the parameters from the match structure
params :: MatchInfo (FunMatch (FunParams t)) -> Params t
params (MatchInfo _ (FunMatch _ _ (FunParams ps))) = ps

-- | Obtain the first parameter in a typed way
firstParam :: MatchInfo (FunMatch (FunParams (t, r))) -> Val t
firstParam m = case params m of
  ParamsCons v _ -> v

-- | Obtain the second parameter in a typed way
secondParam :: MatchInfo (FunMatch (FunParams (a, (t, r)))) -> Val t
secondParam m = case params m of
  ParamsCons _ (ParamsCons v _) -> v


--
-- Instrumentation monad
-- Combination of a State monad, writer monad, and continuation monad.
--

newtype Instr a = Instr (Int -> (Int -> a -> L.Instr -> (L.Instr, Int)) -> (L.Instr, Int))

runInstr :: Instr a -> L.Spec
runInstr (Instr f) = L.Spec_Instr $ fst $ f 1 (\n _ l -> (l, n))

-- | Functor instance is just handy to have
instance Functor Instr where
  fmap f (Instr g) = Instr (\n k -> g n (\m v -> k m (f v)))

-- | Monad instance provides meta-programming via sequences.
instance Monad Instr where
  return x = Instr $ \n k -> k n x L.Instr_Nop
  (Instr m1) >>= f = Instr $ \n1 k ->
    m1 n1 $ \n2 v1 i1 ->
      let (Instr m2) = f v1
      in m2 n2 $ \n3 v2 i2 -> k n3 v2 (optSeq i1 i2)
  fail s = Instr $ \n k -> k n (error s) (L.Instr_Fail (Just s))

-- | Applicative instance provides sequencing.
--   Can be defined in terms of the monad; the current implementation has slightly less
--   Haskell overhead.
instance Applicative Instr where
  pure x = return x
  (Instr m1) <*> (Instr m2) = Instr $ \n1 k ->
    m1 n1 $ \n2 f i1 ->
      m2 n2 $ \n3 v i2 ->
        k n3 (f v) (L.Instr_Seq i1 i2)

-- | Alternative instance provides parallel composition.
instance Alternative Instr where
  empty = Instr $ \n1 k -> k n1 (error "Alternative:empty") (L.Instr_Fail Nothing)
  (<|>) = alt False True  -- try both alternatives; share continuation

-- | Some variations on the parallel composition.
infixl 3 <<|>
(<<|>) :: Instr a -> Instr a -> Instr a
(<<|>) = alt False False  -- doesn't do right alternative if left one succeeds

infixl 2 <#>
infixl 2 <<#>

(<#>), (<<#>) :: Instr a -> Instr a -> Instr a
(<#>)  = alt True True    -- try both alternatives; duplicate continuation
(<<#>) = alt True False   -- doesn't do right alternative if left one succeeds

-- | Different strategies for combining instrumentations in parallel.
alt :: Bool -> Bool -> Instr a -> Instr a -> Instr a
alt dupCont mode (Instr m1) (Instr m2) = Instr $ \n1 k ->
  if dupCont
  then let (i1,n2) = m1 n1 k
           (i2,n3) = m2 n2 k
       in (optAlt mode i1 i2, n3)
  else m1 n1 $ \n2 v1 i1 ->
         m2 n2 $ \n3 v2 i2 -> k n3 v2 (optAlt mode i1 i2)

-- | Smart constructor for sequencing: optimizes 'Nop' and 'Fail' instrumentations away.
optSeq :: L.Instr -> L.Instr -> L.Instr
optSeq L.Instr_Nop q = q
optSeq p L.Instr_Nop = p
optSeq (L.Instr_Fail msg) q = L.Instr_Fail msg
-- optSeq p (L.Instr_Fail msg) = L.Instr_Fail msg  -- this optimization may only be done if |p| does not have side effect
optSeq p q = L.Instr_Seq p q

-- | Smart constructor for parallel composition: optimizes 'Nop' and 'Fail' instrumentation away.
optAlt :: Bool -> L.Instr -> L.Instr -> L.Instr
optAlt _ (L.Instr_Fail _) q = q
optAlt _ p (L.Instr_Fail _) = p
optAlt _ L.Instr_Nop q = q
optAlt _ p L.Instr_Nop = p
optAlt b p q = L.Instr_Alt p q b

-- | Instrumentation that keeps repeating itself, until it fails. The associated value
--   is the value of the last iteration. Loop never fails: it simply stops executing the body.
--   Loop forever, unless there is side-effect.
--   This is the only instruction that creates cycles in the control-flow of the generated
--   instrumentation. With this combinator, we can capture loops in the generated instrumentation.
loop :: Instr a -> Instr a
loop (Instr m) = Instr $ \n1 k ->
  m n1 $ \n2 v i -> k n2 v (optLoop i)

-- | Eliminates trivial endless loops, and trivial always failing loops
optLoop :: L.Instr -> L.Instr
optLoop (L.Instr_Fail s) = L.Instr_Fail s
optLoop L.Instr_Nop      = L.Instr_Nop
optLoop i                = L.Instr_Loop i


--
-- Primitive combinators
--

-- Provides a fresh integer.
freshInt :: Instr Int
freshInt = Instr $ \n k ->
  let m = n + 1
  in k m n L.Instr_Nop

-- Outputs a core instruction
embed :: L.Instr -> Instr ()
embed l = Instr $ \n k -> k n () l

-- | Primitive operation to invoke a function with a number of parameters.
invokeFun :: FunSpec a b -> Val String -> Params a -> Instr (Params b)
invokeFun (FunSig _ rs) nm inps = do
  outs <- freshParams rs
  let name  = valToVal nm
      inps' = paramsToParams inps
      outs' = paramsToParams outs
  embed (L.Instr_CallFun name inps' outs')
  return outs

-- | Primitive operation to invoke a property with a number of parameters.
invokeProp :: ParamsDescr b => Val (FunSpec a b) -> Params a -> Instr (Params b)
invokeProp prop inps = do
  outs <- outputsFromPropVal prop
  let vProp  = valToVal prop
      inps' = paramsToParams inps
      outs' = paramsToParams outs
  embed (L.Instr_CallProp vProp inps' outs')
  return outs

-- | Derive outputs based on the known type info
outputsFromPropVal :: ParamsDescr b => Val (FunSpec a b) -> Instr (Params b)
outputsFromPropVal _ = paramsFromType

-- | Type level computation only
class ParamsDescr a where
  paramsFromType :: Instr (Params a)

instance ParamsDescr Any where
  paramsFromType = do
    v <- fresh
    return $ ParamsAny v

instance ParamsDescr () where
  paramsFromType = return ParamsEmpty

instance ParamsDescr b => ParamsDescr (a,b) where
  paramsFromType = do
    v  <- fresh
    ps <- paramsFromType
    return (ParamsCons v ps)

-- | Executes |Instr| on all previous matching join points; retains the results of the last executed one.
onPrevious :: Instr a -> Instr a
onPrevious = transform L.Instr_Last

-- | Convert values to core structure.
valToVal :: Val a -> L.Val
valToVal v = case v of
  ValSym (Sym n)    -> L.Val_Sym n
  ValCon con        -> case con of
    ConInt n      -> L.Val_Int n
    ConUInt n     -> L.Val_UInt n
    ConBool b     -> L.Val_Bool b
    ConString s   -> L.Val_String s
    ConArray vs   -> L.Val_Array (map valToVal vs)
    ConType t     -> L.Val_Type (typeToType t)
    ConMethod n _ -> L.Val_Method n
  ValInd v1 v2        -> L.Val_Ind (valToVal v1) (valToVal v2)
  ValProp v (Key s _) -> L.Val_Prop (valToVal v) s
  ValStatic (Key s _) -> L.Val_Static s
  ValDyn v1 v2        -> L.Val_Dyn (valToVal v1) (valToVal v2)

-- | Convert types to core structure.
typeToType :: Type a -> L.Type
typeToType t = case t of
  TypeAny      -> L.Type_Any
  TypeBool     -> L.Type_Bool
  TypeInt      -> L.Type_Int
  TypeUInt     -> L.Type_UInt
  TypeDouble   -> L.Type_Double
  TypeString   -> L.Type_String
  TypeArray t  -> L.Type_Array (typeToType t)
  TypeBase     -> L.Type_Base
  TypeObject s -> L.Type_Object s
  TypeMethod _ -> L.Type_Method

-- | Convert parameters to core structure.
paramsToParams :: Params a -> L.Params
paramsToParams (ParamsAny v)     = L.Params_Any (valToVal v)
paramsToParams ParamsEmpty       = L.Params_Nil
paramsToParams (ParamsCons v ps) = L.Params_Cons (L.Param_Param (valToVal v)) (paramsToParams ps)

-- | Introduces a fresh value for each of the parameters in the specification.
freshParams :: ParamSpecs a -> Instr (Params a)
freshParams SpecsAny        = fresh >>= return . ParamsAny
freshParams SpecsEmpty      = return ParamsEmpty
freshParams (SpecsCons _ r) = do
  v  <- fresh
  ps <- freshParams r
  return (ParamsCons v ps)


-- Matches

-- | Matches upon the entry of a block
matchBlockEntry :: Instr (MatchInfo BlockMatch)
matchBlockEntry = do
  vId   <- fresh
  vCyc  <- fresh
  vExpt <- fresh
  embed $ L.Instr_Match $ L.Match_EnterBlock (valToVal vId) (valToVal vCyc) (valToVal vExpt)
  return $ MatchInfo vId $ BlockMatch vCyc vExpt

-- | Matches against a block that succeeded
matchBlockDone :: Instr (MatchInfo BlockMatch)
matchBlockDone = do
  vId <- fresh
  vCyc <- fresh
  let vExpt = ValCon ConNothing
  embed $ L.Instr_Match $ L.Match_LeaveBlock (valToVal vId) (valToVal vCyc)
  return $ MatchInfo vId $ BlockMatch vCyc vExpt

-- | Matches against a block that fails
matchBlockFail :: Instr (MatchInfo BlockMatch)
matchBlockFail = do
  vId   <- fresh
  vExpt <- fresh
  let vCyc = con False
  let vMbExpt = ValCon $ ConJust vExpt
  embed $ L.Instr_Match $ L.Match_FailBlock (valToVal vId) (valToVal vMbExpt)
  return $ MatchInfo vId $ BlockMatch vCyc vMbExpt

-- | Matches against the entry of a coercion.
matchCoerceEnter :: Coercion a b -> Instr (MatchInfo (CoerceMatch a b))
matchCoerceEnter CoerceNone = fail "a none coercion never matches"
matchCoerceEnter coe = do
  vId  <- fresh
  vVal <- fresh
  embed $ L.Instr_Match $ L.Match_BeginCoerce (valToVal vId) (valToVal vVal) (coeToCoe coe)
  return $ MatchInfo vId $ CoerceMatch coe $ CoerceEntry vVal

-- | Matches when a coercion succeeded.
matchCoerceDone :: Coercion a b -> Instr (MatchInfo (CoerceMatch a b))
matchCoerceDone CoerceNone = fail "a none coercion never matches"
matchCoerceDone coe = do
  vId  <- fresh
  vVal <- fresh
  embed $ L.Instr_Match $ L.Match_DoneCoerce (valToVal vId) (valToVal vVal) (coeToCoe coe)
  return $ MatchInfo vId $ CoerceMatch coe $ CoerceDone vVal

-- | Matches when a coercion failed.
matchCoerceFail :: Coercion a b -> Instr (MatchInfo (CoerceMatch a b))
matchCoerceFail CoerceNone = fail "a none coercion never matches"
matchCoerceFail coe = do
  vId   <- fresh
  vInp  <- fresh
  vExpt <- fresh
  embed $ L.Instr_Match $ L.Match_FailedCoerce (valToVal vId) (valToVal vExpt) (valToVal vInp) (coeToCoe coe)
  return $ MatchInfo vId $ CoerceMatch coe $ CoerceFail vInp vExpt

-- | Converts a coercion to core representation.
coeToCoe :: Coercion a b -> L.Coercion
coeToCoe coe = case coe of
  CoerceNone      -> L.Coercion_None
  CoerceAny       -> L.Coercion_Any
  CoerceAny'      -> L.Coercion_Any'
  CoerceString    -> L.Coercion_String
  CoerceDouble    -> L.Coercion_Double
  CoerceInt       -> L.Coercion_Int
  CoerceUInt      -> L.Coercion_UInt
  CoerceInstance  -> L.Coercion_Instance
  

-- | Match entry of a function.
matchFunEnter :: InputSpecs p a => p -> Instr (MatchInfo (FunMatch (FunParams a)))
matchFunEnter specs = mFunOk (inputSpecs specs) L.Match_EnterFun

-- | Match successfully leaving of a function.
matchFunDone :: OutputSpecs p b => p -> Instr (MatchInfo (FunMatch (FunParams b)))
matchFunDone specs = mFunOk (outputSpecs specs) L.Match_LeaveFun

-- | Match a function that exits with an exception.
matchFunFail :: Instr (MatchInfo (FunMatch FunFail))
matchFunFail = mFunFail L.Match_FailFun

-- | Match entry of a function.
matchCallEnter :: InputSpecs p a => p -> Instr (MatchInfo (FunMatch (FunParams a)))
matchCallEnter specs = mFunOk (inputSpecs specs) L.Match_BeginCall

-- | Match successfully leaving of a function.
matchCallDone :: OutputSpecs p b => p -> Instr (MatchInfo (FunMatch (FunParams b)))
matchCallDone specs = mFunOk (outputSpecs specs) L.Match_DoneCall

-- | Match a function that exits with an exception.
matchCallFail :: Instr (MatchInfo (FunMatch FunFail))
matchCallFail = mFunFail L.Match_FailedCall

-- | Matches against several forms of function invocation/return.
mFunOk :: ParamSpecs a -> (L.Val -> L.Val -> L.Val -> L.Params -> L.Match) -> Instr (MatchInfo (FunMatch (FunParams a)))
mFunOk specs f = do
  vId   <- fresh
  vName <- fresh
  vKind <- fresh
  ps    <- freshParams specs
  embed $ L.Instr_Match $ f (valToVal vId) (valToVal vName) (valToVal vKind) (paramsToParams ps)
  return $ MatchInfo vId $ FunMatch vName vKind $ FunParams ps

-- | Matches against several forms of failed functions.
mFunFail :: (L.Val -> L.Val -> L.Val -> L.Val -> L.Match) -> Instr (MatchInfo (FunMatch FunFail))
mFunFail f = do
  vId   <- fresh
  vName <- fresh
  vKind <- fresh
  excpt  <- fresh
  embed $ L.Instr_Match $ f (valToVal vId) (valToVal vName) (valToVal vKind) (valToVal excpt)
  return $ MatchInfo vId $ FunMatch vName vKind $ FunFail excpt

-- | Match entry of a property
matchPropEnter :: Key o (FunSpec a b) -> Instr (MatchInfo (FunMatch (FunParams a)))
matchPropEnter k = do
  m <- matchFunEnter k
  assertProp m k
  return m

-- | Match successfully leaving of a property.
matchPropDone :: Key o (FunSpec a b) -> Instr (MatchInfo (FunMatch (FunParams b)))
matchPropDone k = do
  m <- matchFunDone k
  assertProp m k
  return m

-- | Match a property that exits with an exception.
matchPropAbort :: Instr (MatchInfo (FunMatch FunFail))
matchPropAbort = matchFunFail

-- | Match entry of a property call.
matchPropCall :: Key o (FunSpec a b) -> Instr (MatchInfo (FunMatch (FunParams a)))
matchPropCall k = do
  m <- matchCallEnter k
  assertProp m k
  return m

-- | Match successfully leaving of a property call.
matchPropReturn :: Key o (FunSpec a b) -> Instr (MatchInfo (FunMatch (FunParams b)))
matchPropReturn k = do
  m <- matchCallDone k
  assertProp m k
  return m

-- | Match a property call that exits with an exception.
matchPropFail :: Instr (MatchInfo (FunMatch FunFail))
matchPropFail = matchCallFail

-- | Asserts if a function match matches a certain prop identified by the given key.
assertProp :: MatchInfo (FunMatch c) -> Key o (FunSpec a b) -> Instr ()
assertProp m k = assert (isProp m k)

isProp :: MatchInfo (FunMatch c) -> Key o (FunSpec a b) -> Expr Bool
isProp m (Key nm _) = (ExprVal $ funName $ matchInfo m) .==. (ExprVal $ con nm)

-- | Applies a transformation function on the compiled instrumentation.
transform :: (L.Instr -> L.Instr) -> Instr a -> Instr a
transform f (Instr m) = Instr $ \n1 k -> m n1 (\n2 v i -> k n2 v (f i))

-- | Instrumentation is deferred to runtime.
dynamic :: Instr a -> Instr a
dynamic = transform L.Instr_Dyn

-- | Instrumentation must be resolved at instrumentation time.
static :: Instr a -> Instr a
static = transform L.Instr_Static

-- | Tests if the value has a |True| value: succeeds if this is the case, fails if not.
assertValTrue :: Val Bool -> Instr ()
assertValTrue v = embed $ L.Instr_Assert $ valToVal v

-- | Runs an instrumentation on predecessors; returns the result of the last one, or fails.
predecessors :: Instr a -> Instr a
predecessors = transform L.Instr_Last


-- | Expression to instrumentation. The instrumentation succeeds if the
-- expression evaluates to a value, then returns that value. Since the
-- expression may contain nested instrumentations, the evaluation of
-- expressions may fail.
expr :: IsExpr p a => p -> Instr (Val a)
expr = expr' . toExpr

-- | Compiles the expression to instrumentations.
expr' :: Expr a -> Instr (Val a)
expr' e = case e of
  ExprVal v      -> return v
  ExprUn op e'   -> do
    vIn <- expr' e'
    vOut <- fresh
    embed $ L.Instr_UnOp (valToVal vIn) (valToVal vOut) (unopToUnop op)
    return vOut
  ExprBin op l r -> do
    vL   <- expr' l
    vR   <- expr' r
    vOut <- fresh
    embed $ L.Instr_BinOp (valToVal vL) (valToVal vR) (valToVal vOut) (binopToBinop op)
    return vOut
  ExprInstr m    -> m

-- | Converts a unary operator to its core representation.
unopToUnop :: UnOp a b -> L.UnOp
unopToUnop op = case op of
  OpAbs       -> L.UnOp_Abs
  OpNeg       -> L.UnOp_Neg
  OpNot       -> L.UnOp_Not
  OpIsJust    -> L.UnOp_IsJust
  OpIsNothing -> L.UnOp_IsNothing
  OpExtract   -> L.UnOp_ExtractJust
  OpLength    -> L.UnOp_Length

-- | Converts a binary operator to its core representation.
binopToBinop :: BinOp a b c -> L.BinOp
binopToBinop op = case op of
  OpAnd -> L.BinOp_And
  OpOr  -> L.BinOp_Or
  OpRel r -> L.BinOp_Rel (relToRel r)  -- relation operator
  OpAdd -> L.BinOp_Add
  OpSub -> L.BinOp_Sub
  OpMul -> L.BinOp_Mul
  OpDiv -> L.BinOp_Div
  OpMod -> L.BinOp_Mod
  OpMax -> L.BinOp_Max
  OpMin -> L.BinOp_Min

-- | Converts a BinRel to the underlying representation.
relToRel :: BinRel a -> L.Rel
relToRel rel = case rel of
  RelEqual         -> L.Rel_Equal
  RelSmaller       -> L.Rel_Smaller
  RelSmallerEqual  -> L.Rel_SmallerEqual
  RelGreater       -> L.Rel_Greater
  RelGreaterEqual  -> L.Rel_GreaterEqual

-- | Returns the type of a value.
typeofVal :: Val a -> Instr (Val (Type a))
typeofVal v = do
  v' <- fresh
  embed $ L.Instr_TypeOf (valToVal v) (valToVal v')
  return v'

-- | Coerces the value of some type to another type.
coerceVal :: Coercion a b -> Val a -> Instr (Val b)
coerceVal CoerceNone v = return v
coerceVal coe v = do
  v' <- fresh
  let coe' :: L.Coercion
      coe' = case coe of
               CoerceAny      -> L.Coercion_Any
               CoerceAny'     -> L.Coercion_Any'
               CoerceString   -> L.Coercion_String
               CoerceDouble   -> L.Coercion_Double
               CoerceInt      -> L.Coercion_Int
               CoerceUInt     -> L.Coercion_UInt
               CoerceInstance -> L.Coercion_Instance
  embed $ L.Instr_Coerce coe' (valToVal v) (valToVal v')
  return v'

-- | Assigns a value to a variable.
assignVal :: Var a -> Val a -> Instr ()
assignVal (Sym n) v = embed $ L.Instr_Assign n (valToVal v)

-- | Static type assumption
assumeType :: Val a -> Val (Type a) -> Instr ()
assumeType v t = embed (L.Instr_Type (valToVal v) (valToVal t))


--
-- Derived instrumentations
--


-- | Calls a property that may take a tuple of expressions as parameters.
-- type signature for v required for inference
callProp :: forall a b e p . (IsExprs a p, IsExpr e (FunSpec a b), ParamsDescr b) => e -> p -> Instr (Params b)
callProp e ps = do
  v <- (expr e :: Instr (Val (FunSpec a b)))
  ps' <- exprsToParams $ toExprs ps
  invokeProp v ps'

-- | Ignores return value
callProp' :: forall a b e p . (IsExprs a p, IsExpr e (FunSpec a b), ParamsDescr b) => e -> p -> Instr ()
callProp' e ps = callProp e ps >> return ()

-- | Obtain signature from a property.
staticProp :: Key o (FunSpec a b) -> Expr (FunSpec a b)
staticProp = ExprVal . ValStatic

-- | Calls a function that may take a tuple of expressions as parameters.
callFun :: (IsExprs a p, IsExpr s String) => FunSpec a b -> s -> p -> Instr (Params b)
callFun f nm ps = do
    nm' <- expr $ toExpr nm
    ps' <- exprsToParams $ toExprs ps
    invokeFun f nm' ps'

-- | Tests if some condition is met; if the condition is 'False', evaluation aborts.
assert :: IsExpr a Bool => a -> Instr ()
assert g = do
  v <- expr $ toExpr g
  assertValTrue v

assertAll :: IsExpr a Bool => [a] -> Instr ()
assertAll gs = mapM_ assert gs

-- | Tests if two expressions return equal values.
equal :: (IsExpr p Bool, IsExpr q Bool) => p -> q -> Instr ()
equal p q = assert ((.==.) p q)

-- | Assignment of a value to a variable.
infix 2 .=
(.=) :: IsExpr p a => Var a -> p -> Instr ()
ref .= e = expr (toExpr e) >>= assignVal ref

-- | Introduces a fresh, initialized variable.
var :: IsExpr p a => p -> Instr (Var a)
var e = do ref <- freshVar
           ref .= e
           return ref

-- | If-then-else instrumentation.
ifte :: (IsExpr g Bool, IsExpr p a, IsExpr q a) => g -> p -> q -> Instr (Expr a)
ifte g t e = fmap ExprVal (ifte' g t e)

ifte' :: (IsExpr g Bool, IsExpr p a, IsExpr q a) => g -> p -> q -> Instr (Val a)
ifte' g t e = (assert g >> expr t) <<|> expr e

-- | While-loop (reevaluates 'g' for each iteration)
while :: IsExpr g Bool => g -> Instr a -> Instr a
while g m = loop (assert g >> m)

-- | For-loop
for :: (IsNumeric a, IsExpr p a) => p -> (Expr a -> Expr Bool) -> (Expr a -> Expr a) -> (Expr a -> Instr b) -> Instr b
for i g u b = do
  x <- var i
  let e = ExprVal $ ValSym x
  while (g e) $ do
    v <- b e
    x .= u e
    return v


--
-- Derived expressions
--

-- | Produces a fresh value.
fresh :: Instr (Val a)
fresh = fmap ValSym freshVar

-- | Produces a fresh variable.
freshVar :: Instr (Var a)
freshVar = fmap Sym freshInt

-- | Nested instrumentation (no result).
nest0 :: Instr () -> Expr Bool
nest0 m = nest $ fmap (const (con True)) m

-- | Nested instrumentation (with value as result).
nest :: Instr (Val a) -> Expr a
nest = ExprInstr

-- | Function call without a result value.
call0 :: (IsExprs a p, IsExpr s String) => FunSpec a () -> s -> p -> Expr Bool
call0 f nm ps = nest0 $ fmap (const ()) $ callFun f nm ps

-- | Function call with a single result value.
call :: (IsExprs a p, IsExpr s String) => FunSpec a (b, ()) -> s -> p -> Expr b
call f nm ps = nest $ fmap extract $ callFun f nm ps where
  extract :: Params (b, ()) -> Val b
  extract (ParamsCons v ParamsEmpty) = v

-- | Turns a expression product into a val-product.
exprsToParams :: Exprs a -> Instr (Params a)
exprsToParams ExprsEmpty = return ParamsEmpty
exprsToParams (ExprsCons e es) = do
  v  <- expr e
  fmap (ParamsCons v) $ exprsToParams es

-- | Casts one value into another value (implicitly).
cast :: (IsExpr a b, HasCoercion b c) => a -> Expr c
cast = coerce mkCoercion

-- | Coerces a value into another value (explicitly).
coerce :: IsExpr a b => Coercion b c -> a -> Expr c
coerce c e = nest (expr (toExpr e) >>= coerceVal c)

-- | Obtains the type of an expression.
typeof :: IsExpr a b => a -> Expr (Type b)
typeof e = nest (expr (toExpr e) >>= typeofVal)

-- | If-then-else expression.
ite :: (IsExpr g Bool, IsExpr p a, IsExpr q a) => g -> p -> q -> Expr a
ite g t e = nest (ifte' g t e)

-- | Common case for binary operators.
bin :: (IsExpr p a, IsExpr q b) => BinOp a b c -> p -> q -> Expr c
bin o p q = ExprBin o (toExpr p) (toExpr q)

-- | Common case for unary operators.
una :: IsExpr p a => UnOp a b -> p -> Expr b
una o p = ExprUn o (toExpr p)

infix 8 .!.
(.!.) :: (IsSuper a b, IsExpr p b) => p -> Key a c -> Expr c
p .!. k = ExprInstr $ fmap (flip ValProp k) $ expr p


--
-- Convenience operators
--

unNot :: IsExpr p Bool => p -> Expr Bool
unNot = una OpNot

unAbs :: (IsExpr p a, IsNumeric a) => p -> Expr a
unAbs = una OpAbs

unNeg :: (IsExpr p a, IsNumeric a) => p -> Expr a
unNeg = una OpNeg

-- Check if the value is a nothing-value.
unIsNothing :: (IsExpr p (Maybe a)) => p -> Expr Bool
unIsNothing = una OpIsNothing

-- | Check if the value is a just-value.
unIsJust :: (IsExpr p (Maybe a)) => p -> Expr Bool
unIsJust = una OpIsJust

-- | Get the value stored in a just-value.
unExtract :: (IsExpr p (Maybe a)) => p -> Expr a
unExtract = una OpExtract

-- | Get the length of an array.
unLength :: (IsExpr p [a]) => p -> Expr Int
unLength = una OpLength


infix 4 .==., .<=., .<., .>=., .>., ./=.
(.==.), (./=.) :: (IsExpr p a, IsExpr q a) => p -> q -> Expr Bool
(.==.) = bin (OpRel RelEqual)
(./=.) p q = unNot (p .==. q)

(.<=.), (.<.), (.>=.), (.>.) :: (IsExpr p a, IsExpr q a, IsComparable a) => p -> q -> Expr Bool
(.<=.) = bin (OpRel RelSmallerEqual)
(.<.)  = bin (OpRel RelSmaller)
(.>=.) = bin (OpRel RelGreaterEqual)
(.>.)  = bin (OpRel RelGreater)

infixr 3 .&&.
infixr 2 .||.
(.||.), (.&&.) :: (IsExpr p Bool, IsExpr q Bool) => p -> q -> Expr Bool
(.||.) = bin OpOr
(.&&.) = bin OpAnd

infixl 6 .+., .-.
infixl 7 .*., ./., .%.
infixl 5 ./\., .\/.

(.+.), (.-.), (.*.), (./.), (./\.), (.\/.) :: (IsNumeric a, IsExpr p a, IsExpr q a) => p -> q -> Expr a
(.+.)  = bin OpAdd
(.-.)  = bin OpSub
(.*.)  = bin OpMul
(./.)  = bin OpDiv
(./\.) = bin OpMax
(.\/.) = bin OpMin

(.%.) :: (IsNumInt a, IsExpr p a, IsExpr q a) => p -> q -> Expr a
(.%.) = bin OpMod
