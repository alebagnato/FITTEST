-- diff

--
-- DSL to script transformations
--

{-# LANGUAGE GADTs, KindSignatures, EmptyDataDecls, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module ByteCodeTrfDSL where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Applicative
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.Word
import ByteCode
import ProgInfo
import ByteCodeTrf
import ByteCodeSupport
import Codec.Binary.UTF8.String
import Data.ByteString.Lazy(ByteString,unpack)
import qualified Data.ByteString.Lazy as L
import Options
import Debug.Trace


-- Code injection monad
--
-- Because it is wrapped in a newtype, we define many instances directly
-- in terms of the unwrapped code. These instances make the injection monad
-- easier to use.

newtype Inj a = Inj { unInj :: RWST CtxInfo InjExpr StateInfo (ErrorT String Identity) a }

instance Monad Inj where
  return  = Inj . return
  fail    = Inj . fail
  m >>= f = Inj (unInj m >>= unInj . f)

instance Functor Inj where
  f `fmap` m = Inj (f `fmap` unInj m)

instance MonadWriter InjExpr Inj where
  tell    = Inj . tell
  listen  = Inj . listen . unInj
  pass    = Inj . pass . unInj

instance MonadReader CtxInfo Inj where
  ask     = Inj ask
  local f = Inj . local f . unInj

instance MonadState StateInfo Inj where
  get     = Inj get
  put     = Inj . put

instance MonadPlus Inj where
  mzero   = Inj mzero
  p `mplus` q = Inj (unInj p `mplus` unInj q)

instance MonadError String Inj where
  throwError     = Inj . throwError
  catchError m h = Inj $ catchError (unInj m) (unInj . h)

instance Applicative Inj where
  pure    = return
  p <*> q = p >>= \f -> q >>= return . f

instance Alternative Inj where
  empty   = mzero
  (<|>)   = mplus


infixl 4 <#>

-- | Run both and return the value of the right-most one that succeeds
(<#>) :: Inj a -> Inj a -> Inj a
p <#> q = (p >>= \v -> q `catchError` (const $ return v)) `catchError` (const q)


-- the chained state
data StateInfo = StateInfo
  { stateUid :: Int
  , stateLab :: Maybe (Value LabelRef)
  }

-- Values are placeholders for the actual references.
data Value :: * -> * where
  ValueLabel     :: Int -> Value LabelRef
  ValueLocal     :: Int -> Value LocalRef
  ValueInt       :: Int -> Value IntRef
  ValueUInt      :: Int -> Value UIntRef
  ValueDouble    :: Int -> Value DoubleRef
  ValueString    :: Int -> Value StringRef
  ValueNamespace :: Int -> Value NamespaceRef
  ValueName      :: Int -> Value NameRef
  ValueConst     :: a   -> Value a

-- Wrap any type a as an Value
constE :: a -> Value a
constE = ValueConst

data Local
type LabelRef = Int
type LocalRef = Ref Local

-- prepend or append instructions
data InjectMode
  = InjPrepend
  | InjAppend

modeIsAppend :: InjectMode -> Bool
modeIsAppend InjPrepend = False
modeIsAppend InjAppend  = True

-- Evaluation
-- Note the explicit threading of the unique ids.. this is needed to guarantee
-- that the uids are unique per MethodBody.
injectWithMode :: InjectMode -> CtxInfo -> Inj a -> Int -> (InjRoot, Int)
injectWithMode mode info m uid0 = output where
  rws      = unInj m
  inh      = info
  chn      = StateInfo uid0 Nothing
  e        = runIdentity $ runErrorT $ execRWST rws inh chn
  mkRoot   = InjRoot_Root (modeIsAppend mode)
  output   = case e of
    Left _                       -> (mkRoot InjExpr_Empty, uid0)
    Right (StateInfo uid1 _, t)  -> (mkRoot t, uid1)

inject :: CtxInfo -> Inj () -> Int -> (InjRoot, Int)
inject = injectWithMode InjPrepend


-- lookup based on Value
fetch :: InjEnv -> Value a -> a
fetch env e = case e of
  ValueLabel u -> case lk u of
    ValLabel n -> n
  ValueLocal u -> case lk u of
    ValLocal n -> Ref n
  ValueString u -> case lk u of
    ValString r -> r
  ValueInt   u -> case lk u of
    ValInt r  -> r
  ValueUInt  u -> case lk u of
    ValUInt r -> r
  ValueDouble u -> case lk u of
    ValDouble r -> r
  ValueNamespace u -> case lk u of
    ValNamespace r -> r
  ValueName u -> case lk u of
    ValName r -> r
  ValueConst v -> v
  where lk k = let msg = "fetch::key " ++ show k ++ " not in map " ++ shw0 (IntMap.assocs env)
                   shw0 []     = "."
                   shw0 (x:xs) = shw x ++ shw1 xs
                   shw1 []     = "."
                   shw1 (x:xs) = ", " ++ shw x ++ shw1 xs
                   shw (n,v)   = show n ++ " -> " ++ show v
               in IntMap.findWithDefault (error msg) k env

--
-- Querying the context of the currect joint point
--

currentCtx :: Inj CtxInfo
currentCtx = ask

currentTbls :: Inj SymbolTables
currentTbls = asks ctxTbls

currentBlockId  :: Inj Int
currentBlockId = asks (ctxBlockId . ctxBlock)

currentMethodId :: Inj Word32
currentMethodId = asks (ctxMethodId . ctxMethod)

currentParents :: Inj CtxParents
currentParents = asks ctxParents

currentMethodName :: Inj QName
currentMethodName = parentsToMethodName <$> currentParents
{-
do
  parents <- currentParents
  return (parentsToMethodName parents)
-}

-- query the options passed from the top-level
options :: Inj Options
options = asks ctxOpts


getMethodName :: Inj String
getMethodName = do
  nm_  <- currentMethodName
  mid_ <- currentMethodId
  nm   <- return . showByteStr . qName $ nm_
  if null nm then return . show . toInteger $ mid_ else return nm
  
getClassName :: Inj String
getClassName = do
   parents <- currentParents
   case parents of
      (CtxParentsCons (CtxTrait (CtxObjClass clsref) _ _ _) _) 
          -> do
             symtab <- currentTbls
             let className_ = classQName symtab clsref
             return . showByteStr . qName $ className_
      -- if no match return empty string:
      _   -> return ""

           


-- to calculate how many arguments a function has:
getNumOfArgs :: Inj Int
getNumOfArgs = do
   methodRefId <- currentMethodId
   symtab <- currentTbls
   return . length . sigParams . lookupMethod (Ref methodRefId) $ symtab
   
-- to get the return type of a function:   
getRetType :: Inj String
getRetType = do
   methodRefId <- currentMethodId
   symtab <- currentTbls
   retTyId <- return . sigReturn . lookupMethod (Ref methodRefId) $ symtab
   retTy_  <- return . nmStr . lookupName retTyId $ symtab
   case retTy_ of
      Just z -> return . lookupString z $ symtab
      _      -> return ""

--
-- Basic operations to build an injection
-- 

getUid :: Inj Int
getUid = do
  u <- gets stateUid
  modify (\s -> s { stateUid = stateUid s + 1 })
  return u

debug :: Show a => a -> Inj ()
debug msg = Inj (return ()) -- Inj (trace (show msg) (return ()))


freshLabel :: Inj (Value LabelRef)
freshLabel = do
  uid <- getUid
  tell $ InjExpr_DefLabel uid
  return (ValueLabel uid)

freshLocal :: Inj (Value LocalRef)
freshLocal = do
  uid <- getUid
  tell $ InjExpr_DefLocal uid
  return (ValueLocal uid)

declInt :: Int -> Inj (Value IntRef)
declInt val = do
  uid <- getUid
  tell $ InjExpr_DefInt uid $ fromIntegral val
  return (ValueInt uid)

declInt1 :: Word32 -> Inj (Value IntRef)
declInt1 val = do
  uid <- getUid
  tell $ InjExpr_DefInt uid val
  return (ValueInt uid)

declUInt :: Word32 -> Inj (Value UIntRef)
declUInt val = do
  uid <- getUid
  tell $ InjExpr_DefUInt uid val
  return (ValueUInt uid)

declDouble :: Double -> Inj (Value DoubleRef)
declDouble val = do
  uid <- getUid
  tell $ InjExpr_DefDouble uid val
  return (ValueDouble uid)

class    OpDeclString t                 where  declString :: t -> Inj (Value StringRef)
instance OpDeclString String            where  declString = declString1
instance OpDeclString ByteString        where  declString = declString2
instance OpDeclString (Value StringRef) where  declString = return
instance OpDeclString StringRef         where  declString = return . ValueConst

declString1 :: String -> Inj (Value StringRef)
declString1 val = do
  let val' = L.pack $ encode val
  uid <- getUid
  tell $ InjExpr_DefString uid val'
  return (ValueString uid)

declString2 :: ByteString -> Inj (Value StringRef)
declString2 val = do
  uid <- getUid
  tell $ InjExpr_DefString uid val
  return (ValueString uid)

class    IsString t                 where toStringRef :: t -> Value StringRef
instance IsString (Value StringRef) where toStringRef = id
instance IsString (Ref String)      where toStringRef = ValueConst

declNsGeneral :: OpDeclString t => t -> Inj (Value NamespaceRef)
declNsGeneral = declNs_ NamespaceKind_General

declNsKind :: NamespaceKind -> Inj (Value NamespaceRef)
declNsKind kind = declNs_ kind L.empty

declNs_ :: OpDeclString t => NamespaceKind -> t -> Inj (Value NamespaceRef)
declNs_ kind str = do
  strE <- declString str
  uid <- getUid
  tell $ InjExpr_DefNamespace uid $ \env ->
    let strR = fetch env strE
    in NamespaceInfo_Info kind (refVal strR)
  return (ValueNamespace uid)

declNsFull :: NamespaceInfo -> Inj (Value NamespaceRef)
declNsFull info = do
  uid <- getUid
  tell $ InjExpr_DefNamespace uid $ const info
  return (ValueNamespace uid)

class    OpDeclNs t                    where declNs :: t -> Inj (Value NamespaceRef)
instance OpDeclNs String               where declNs = declNsGeneral
instance OpDeclNs ByteString           where declNs = declNsGeneral
instance OpDeclNs NamespaceKind        where declNs = declNsKind
instance OpDeclNs (Value NamespaceRef) where declNs = return
instance OpDeclNs NamespaceRef         where declNs = return . ValueConst

declQName_ :: (OpDeclNs n, OpDeclString s) => Bool -> n -> s -> Inj (Value NameRef)
declQName_ isAttrNm ns str = do
  nsE  <- declNs ns
  strE <- declString str
  uid <- getUid
  tell $ InjExpr_DefName uid $ \env ->
    let nsR  = fetch env nsE
        strR = fetch env strE
        f    = if isAttrNm then MultinameInfo_QNameA else MultinameInfo_QName
    in f (refVal nsR) (refVal strR)
  return (ValueName uid)

declQName :: (OpDeclNs n, OpDeclString s) => n -> s -> Inj (Value NameRef)
declQName = declQName_ False

declQNameA :: (OpDeclNs n, OpDeclString s) => n -> s -> Inj (Value NameRef)
declQNameA = declQName_ True

declName_ :: OpDeclString s => Bool -> s -> Inj (Value NameRef)
declName_ isAttr = declQName_ isAttr L.empty

declNameA :: OpDeclString s => s -> Inj (Value NameRef)
declNameA = declName_ True

declName :: OpDeclString s => s -> Inj (Value NameRef)
declName  = declName_ False

class    IsName t               where toNameRef :: t -> Value NameRef
instance IsName (Value NameRef) where toNameRef = id
instance IsName (Ref Name)      where toNameRef = ValueConst

pushInstr :: LabInstruction -> Inj ()
pushInstr instr = tell $ InjExpr_PushInstr $ const instr


-- Label related instructions

class IsLabel t where toLabelRef :: t -> Value LabelRef
instance IsLabel (Value LabelRef) where toLabelRef = id
instance IsLabel LabelRef         where toLabelRef = ValueConst

applyLabel :: IsLabel l => l -> Inj ()
applyLabel l = modify (\s -> s { stateLab = Just $ toLabelRef l })

takeLabel :: Inj (Maybe (Value LabelRef))
takeLabel = do
  l <- gets stateLab
  modify (\s -> s { stateLab = Nothing })
  return l

-- add an instruction
push :: (InjEnv -> Instruction) -> Inj ()
push f = do
  mbL <- takeLabel
  l   <- case mbL of
           Nothing -> freshLabel
           Just r  -> return r
  tell $ InjExpr_PushInstr $ \env -> LabInstruction_Instr (fetch env l) (f env)
  return ()


--
-- Combinators to build instructions
--

-- | Pushes a label instruction
label :: IsLabel l => l -> Inj ()
label ref = do
  applyLabel ref
  virtual 0 VirtKind_Label
  lbl

-- | Virtual branch target.
virtual :: Int -> VirtKind -> Inj ()
virtual vid kind = push $ const $ Instruction_Virtual vid kind

call :: Integral n => n -> Inj ()
call = push . const . Instruction_Call . fromIntegral

callProp :: (IsName nm, Integral n) => nm -> n -> Inj ()
callProp nm n = push $ \env -> Instruction_CallProp (refVal $ fetch env $ toNameRef nm) (fromIntegral n)

callPropVoid :: (IsName nm, Integral n) => nm -> n -> Inj ()
callPropVoid nm n = push $ \env -> Instruction_CallPropVoid (refVal $ fetch env $ toNameRef nm) (fromIntegral n)

dup :: Inj ()
dup = push $ const Instruction_Dup

-- The actual label instruction. Not to be confused with annotations.
lbl :: Inj ()
lbl = push $ const Instruction_Label

nop :: Inj ()
nop = push $ const Instruction_Nop

pop :: Inj ()
pop = push $ const Instruction_Pop

pushFalse :: Inj ()
pushFalse = push $ const Instruction_PushFalse

pushTrue :: Inj ()
pushTrue = push $ const Instruction_PushTrue

pushByte :: Int -> Inj()
pushByte b = push $ const (Instruction_PushByte (fromInteger . toInteger $ b)) 

pushInt :: Value IntRef -> Inj()
pushInt i = push $ (\env -> Instruction_PushInt (refVal $ fetch env i)) 

pushUndefined :: Inj ()
pushUndefined = push $ const Instruction_PushUndefined

getGlobalScope :: Inj ()
getGlobalScope = push $ const Instruction_GetGlobalScope

pushString :: IsString t => t -> Inj ()
pushString str = push $ \env -> Instruction_PushString $ refVal $ fetch env $ toStringRef str

returnValue :: Inj ()
returnValue = push $ const Instruction_ReturnValue

returnVoid :: Inj ()
returnVoid = push $ const Instruction_ReturnVoid

branch :: IsLabel l => (Word32 -> Instruction) -> l -> Inj ()
branch f l = push $ \env -> f (fromIntegral $ fetch env r)
  where r = toLabelRef l

jump,ifEq,ifFalse,ifTrue,ifGe,ifGt,ifLe,ifLt,ifNGe,ifNGt,ifNLt,ifNe,ifStrictEq,ifStrictNe :: IsLabel l => l -> Inj ()
jump       = branch Instruction_Jump
ifEq       = branch Instruction_IfEq
ifFalse    = branch Instruction_IfFalse
ifTrue     = branch Instruction_IfTrue
ifGe       = branch Instruction_IfGe
ifGt       = branch Instruction_IfGt
ifLe       = branch Instruction_IfLe
ifLt       = branch Instruction_IfLt
ifNGe      = branch Instruction_IfNGe
ifNGt      = branch Instruction_IfNGt
ifNLt      = branch Instruction_IfNLt
ifNe       = branch Instruction_IfNe
ifStrictEq = branch Instruction_IfStrictEq
ifStrictNe = branch Instruction_IfStrictNe

findProp :: IsName n => n -> Inj ()
findProp nm = push $ \env -> Instruction_FindProperty $ refVal $ fetch env $ toNameRef nm

findPropStrict :: IsName n => n -> Inj ()
findPropStrict nm = push $ \env -> Instruction_FindPropStrict $ refVal $ fetch env $ toNameRef nm

getProp :: IsName n => n -> Inj ()
getProp nm = push $ \env -> Instruction_GetProperty $ refVal $ fetch env $ toNameRef nm

setProp :: IsName n => n -> Inj ()
setProp nm = push $ \env -> Instruction_SetProperty $ refVal $ fetch env $ toNameRef nm

-- get k-th local variable, and push it to the stack:
getLocal :: Int -> Inj()
getLocal k = push (const instr)
  where
  k_ :: Word32
  k_ = fromInteger . toInteger $ k
  instr = case k of
            0 -> Instruction_GetLocal0
            1 -> Instruction_GetLocal1
            2 -> Instruction_GetLocal2
            3 -> Instruction_GetLocal3
            _ -> Instruction_GetLocal k_

-- Pop top of the stack, and put it in the k-th local variable          
setLocal :: Int -> Inj()
setLocal k = push (const instr)
  where
  k_ :: Word32
  k_ = fromInteger . toInteger $ k
  instr = case k of
            0 -> Instruction_SetLocal0
            1 -> Instruction_SetLocal1
            2 -> Instruction_SetLocal2
            3 -> Instruction_SetLocal3
            _ -> Instruction_SetLocal k_
            
            
-- getLex??
getLex :: IsName n => n -> Inj ()
getLex nm = push $ \env -> Instruction_GetLex $ refVal $ fetch env $ toNameRef nm 

-- 
swap_ :: Inj ()
swap_ = push $ const Instruction_Swap

-- setting an injected local-var 
setVirtualLocal :: Int -> Inj()
setVirtualLocal i = virtual 0 (VirtKind_SetLocal i)

-- getting the value of an injected local-var 
getVirtualLocal :: Int -> Inj()
getVirtualLocal i = virtual 0 (VirtKind_GetLocal i)

           
-- Abstractions for method calls

pushGlobalObject :: Inj ()
pushGlobalObject = do
  nm <- declName "trace"  -- global object contains "trace"
  findProp nm

callTrace :: IsString t => t -> Inj ()
callTrace str = do
  nm  <- declName "trace"
  findPropStrict nm
  pushString str
  callPropVoid nm 1

callWriteLog :: IsString t => t -> Inj ()
callWriteLog str = do
  nsLogserver <- declNsGeneral "logserver"
  nmWriteLog <- declQName nsLogserver "WriteLog"
  nmLog <- declName "log"
  findPropStrict nmWriteLog
  getProp nmWriteLog
  pushString str
  callPropVoid nmLog 1
