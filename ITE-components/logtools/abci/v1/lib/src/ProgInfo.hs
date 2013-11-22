module ProgInfo
  ( StringPool,IntPool,UIntPool,DoublePool,NamePool,NamespacePool,NamesetsPool
  , Name(..),Qual(..),Namespace(..),Nameset(..)
  , Sigs,Sig(..),SigParams,SigParam(..)
  , ClassDescr(..), ClassDescrs, TraitDescrs, TraitDescr(..), TraitBody(..)
  , ExceptionDescrs, ExceptionDescr(..), lookupException, ExceptionRef
  , SymbolTables(..)
  , Ref(Ref), StringRef, IntRef, UIntRef, DoubleRef, NameRef, NamesetRef, NamespaceRef, MethodRef, ClassRef, refVal, refNull
  , lookupString, lookupInt, lookupUInt, lookupDouble, lookupName, lookupNameset, lookupNamespace, lookupMethod, lookupClass
  , isPublicNamespace, findClass, nameToKey, nameToKey1, defSearchTables
  , eqTrait, eqName, eqName1, eqString1, eqNs, eqNs1, eqQual
  , toTypeInfo, toTypeDescr, toQName, toByteString
  , TpGr(..), emptyTpGr, toTypeGr, InhInfo(..), supertypes, subtypes, activeTypes, joinTypes
  , allClasses, classQName, isAccessible
  , module Env, module EnvInfo
  , CtxInfo(..), CtxBlockInfo(..), CtxMethodInfo(..), CtxTrait(..), CtxObj(..), CtxParents(..), CtxTraitBind(..)
  , parentsToMethodName
  ) where

import Data.Word
import Env
import ByteCode
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S
import Data.Monoid
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.Graph.Inductive as G
import EnvInfo
import Options


-- Symbol tables
data SymbolTables = SymbolTables
  { tableInts      :: IntPool
  , tableUInts     :: UIntPool
  , tableDoubles   :: DoublePool
  , tableStrings   :: StringPool
  , tableNames     :: NamePool
  , tableSpaces    :: NamespacePool
  , tableSets      :: NamesetsPool
  , tableSigs      :: Sigs
  , tableClasses   :: ClassDescrs
  , tableClasses1  :: Map String ClassDescr  -- search map
  , tableTypeInfo  :: TypeInfo
  , graphTypeInfo  :: TpGr
  }
  deriving Show

type StringPool      = StaticEnv String
type IntPool         = StaticEnv Word32
type UIntPool        = StaticEnv Word32
type DoublePool      = StaticEnv Double
type NamePool        = StaticEnv Name
type NamesetsPool    = StaticEnv Nameset
type NamespacePool   = StaticEnv Namespace
type Sigs            = StaticEnv Sig
type ClassDescrs     = StaticEnv ClassDescr
type ExceptionDescrs = StaticEnv ExceptionDescr


-- | Typed reference to indices in a table
-- | Can this type definition serve as an example of using phantom types?
newtype Ref a = Ref Word32
  deriving (Eq,Ord,Show)

refVal :: Ref a -> Word32
refVal (Ref v) = v

refNull :: Ref a -> Bool
refNull (Ref v) = v == 0

type StringRef    = Ref String
type IntRef       = Ref Int
type UIntRef      = Ref Word32
type DoubleRef    = Ref Double
type NameRef      = Ref Name
type NamesetRef   = Ref Nameset
type NamespaceRef = Ref Namespace
type MethodRef    = Ref Sig
type ClassRef     = Ref ClassDescr
type ExceptionRef = Ref ExceptionDescr


-- | Names of identifiers (rather complex structure)
data Name = Name { nmQual :: !Qual, nmStr :: !(Maybe StringRef) }
  deriving (Eq,Ord,Show)

-- | Qualified prefix of a name.
data Qual
  = QualLate                -- Not available statically
  | QualNs !NamespaceRef    -- Prefixed with a namespace
  | QualNss !NamesetRef     -- Prefixed with namespaces
  | QualOther               -- Unknown qualifier
  deriving (Eq,Ord,Show)

-- | A namespace
data Namespace = Namespace { nsKind :: !NamespaceKind, nsName :: !StringRef }
  deriving (Eq,Ord,Show)

-- | A set of namespaces
data Nameset = Nameset { nsSpaces :: ![NamespaceRef] }
  deriving (Eq,Ord,Show)

-- | Method signature
data Sig = Sig { sigName   :: !(Maybe StringRef)  -- may be nameless
               , sigReturn :: !NameRef            -- a Name
               , sigParams :: !SigParams }        -- list of (name,type)
  deriving (Eq,Ord,Show)

-- | Parameters of a mehod.
type SigParams = [SigParam]

-- | Signature of a parameter of a method. A parameter might be nameless.
data SigParam = SigParam { spName :: !(Maybe StringRef), spType :: !NameRef }
  deriving (Eq,Ord,Show)

-- | Class information
data ClassDescr = ClassDescr
  { clName       :: !NameRef            -- name of class
  , clSuper      :: !(Maybe NameRef)    -- name of superclass (if any)
  , clInterfaces :: ![NameRef]          -- interfaces implemented by class
  , clDynTraits  :: !TraitDescrs        -- dynamic traits of a class
  , clStaTraits  :: !TraitDescrs        -- static traits of a class
  }
  deriving (Eq,Ord,Show)

-- | List of traits
type TraitDescrs = [TraitDescr]

-- | Information about a trait
data TraitDescr = TraitDescr { trName :: !NameRef, trData :: !TraitBody }
  deriving (Eq,Ord,Show)

-- | Data of a trait
data TraitBody
  = TraitMethod  { trMethod :: !MethodRef }
  | TraitField   { trType :: !NameRef }
  | TraitClass   { trClass :: !ClassRef }
  deriving (Eq,Ord,Show)

-- | Exception information
data ExceptionDescr = ExceptionDescr
  { expFrom    :: !Word32
  , expTo      :: !Word32
  , expTarget  :: !Word32
  , expTp      :: !NameRef
  , expName    :: !NameRef
  } deriving (Eq,Ord,Show)


--
-- Lookup operations
--

lookupString :: StringRef -> SymbolTables -> String
lookupString (Ref ref) tbls = lookupEnv ref (tableStrings tbls)

lookupInt :: IntRef -> SymbolTables -> Word32
lookupInt (Ref ref) tbls = lookupEnv ref (tableInts tbls)

lookupUInt :: UIntRef -> SymbolTables -> Word32
lookupUInt (Ref ref) tbls = lookupEnv ref (tableUInts tbls)

lookupDouble :: DoubleRef -> SymbolTables -> Double
lookupDouble (Ref ref) tbls = lookupEnv ref (tableDoubles tbls)

lookupName :: NameRef -> SymbolTables -> Name
lookupName (Ref ref) tbls = lookupEnv ref (tableNames tbls)

lookupNameset :: NamesetRef -> SymbolTables -> Nameset
lookupNameset (Ref ref) tbls = lookupEnv ref (tableSets tbls)

lookupNamespace :: NamespaceRef -> SymbolTables -> Namespace
lookupNamespace (Ref ref) tbls = lookupEnv ref (tableSpaces tbls)

lookupMethod :: MethodRef -> SymbolTables -> Sig
lookupMethod (Ref ref) tbls = lookupEnv ref (tableSigs tbls)

lookupClass :: ClassRef -> SymbolTables -> ClassDescr
lookupClass (Ref ref) tbls = lookupEnv ref (tableClasses tbls)

lookupException :: ExceptionRef -> ExceptionDescrs -> ExceptionDescr
lookupException (Ref ref) = lookupEnv ref

--
-- Search operations
--

-- | This operation is rather ineffecient...
findClass :: Name -> SymbolTables -> Maybe ClassDescr
findClass nm tbls = res where
  key = nameToKey tbls nm
  res = Map.lookup key $ tableClasses1 tbls

defSearchTables :: SymbolTables -> SymbolTables
defSearchTables tbls = res where
  mp  = Map.fromList [ (nameToKey1 tbls (Ref r), v) | (r,v) <- assocsEnv (tableClasses tbls) ]
  res = tbls { tableClasses1 = mp }

--
-- Utility operations
--

allClasses :: SymbolTables -> [ClassRef]
allClasses = map Ref . keysEnv . tableClasses

classQName :: SymbolTables -> ClassRef -> QName
classQName tbls ref = toQName tbls $ clName $ lookupClass ref tbls


--
-- Name to a unique string representation that can be used as
-- index in a table
-- This is also the representation that is internally used
-- by the Flex tools
--

nameToKey1 :: SymbolTables -> NameRef -> String
nameToKey1 tbls ref = nameToKey tbls (lookupName ref tbls)

nameToKey :: SymbolTables -> Name -> String
nameToKey tbls nm = qualToKey tbls (nmQual nm) ++ maybe "" (strToKey1 tbls) (nmStr nm)

strToKey1 :: SymbolTables -> StringRef -> String
strToKey1 = flip lookupString

qualToKey :: SymbolTables -> Qual -> String
qualToKey tbls (QualNs ref)   = qualToKey1 tbls ref
qualToKey tbls (QualNss sRef) = case nsSpaces $ lookupNameset sRef tbls of
  [ref]    -> qualToKey1 tbls ref
  _        -> ""
qualToKey _ _  = ""

qualToKey1 :: SymbolTables -> NamespaceRef -> String
qualToKey1 tbls ref
  | null str  = str
  | otherwise = str ++ ":"
  where str = nsToKey1 tbls ref

nsToKey1 :: SymbolTables -> NamespaceRef -> String
nsToKey1 tbls ref = nsToKey tbls (lookupNamespace ref tbls)

nsToKey :: SymbolTables -> Namespace -> String
nsToKey tbls ns = strToKey1 tbls (nsName ns)


--
-- Utility operations
--

isPublicNamespace :: NamespaceRef -> SymbolTables -> Bool
isPublicNamespace ref tbls = b1 && b2 where
  ns = lookupNamespace ref tbls
  nm = lookupString (nsName ns) tbls
  b1 = nsKind ns == NamespaceKind_General
  b2 = null nm

--
-- Equalities
--

eqTrait :: SymbolTables -> TraitDescr -> TraitDescr -> Bool
eqTrait tbls a b = eqName1 tbls (trName a) (trName b)

eqName1 :: SymbolTables -> NameRef -> NameRef -> Bool
eqName1 tbls a b = eqName tbls (lookupName a tbls) (lookupName b tbls)

eqName :: SymbolTables -> Name -> Name -> Bool
eqName tbls a b = b1 && b2 where
  b1 = eqQual tbls (nmQual a) (nmQual b)
  b2 = case (nmStr a, nmStr b) of
         (Nothing, Nothing)     -> True
         ((Just sA), (Just sB)) -> eqString1 tbls sA sB
         _                      -> False

eqQual :: SymbolTables -> Qual -> Qual -> Bool
eqQual tbls (QualNs a) (QualNs b) = eqNs1 tbls a b
eqQual _ _ _ = False

eqNs1 :: SymbolTables -> NamespaceRef -> NamespaceRef -> Bool
eqNs1 tbls a b = eqNs tbls (lookupNamespace a tbls) (lookupNamespace b tbls)

eqNs :: SymbolTables -> Namespace -> Namespace -> Bool
eqNs tbls a b = b1 && b2 where
  b1 = nsKind a == nsKind b
  b2 = eqString1 tbls (nsName a) (nsName b)

eqString1 :: SymbolTables -> StringRef -> StringRef -> Bool
eqString1 tbls a b = p == q where
  p = lookupString a tbls
  q = lookupString b tbls

--
-- Convert a symbol table into a type table
--

toTypeInfo :: SymbolTables -> TypeInfo
toTypeInfo tbls = output where
  output = TypeInfo $ Map.fromList types
  types  = [ (tpName descr, descr) | c <- valuesEnv (tableClasses tbls), let descr = toTypeDescr tbls c ]

toTypeDescr :: SymbolTables -> ClassDescr -> TypeDescr
toTypeDescr tbls c =
  TypeDescr (toQName tbls $ clName c) (fmap (toQName tbls) $ clSuper c)
            (map (toQName tbls) $ clInterfaces c)
            (map toTrait $ clDynTraits c) (map toTrait $ clStaTraits c)
  where
    toTrait t =
      let name = toQName tbls (trName t)
      in TpTraitDescr name (toTraitBody $ trData t)

    toTraitBody body = case body of
      TraitMethod sigRef ->
        let sig = lookupMethod sigRef tbls
        in TrMethod (toSig sig)
      TraitField tpRef   ->
        let tp = toQName tbls tpRef
        in TrField tp
      TraitClass clRef   ->
        let c  = lookupClass clRef tbls
            nm = toQName tbls $ clName c
        in TrClass nm

    toSig sig =
      let params = map toParam $ sigParams sig
          ret    = toQName tbls $ sigReturn sig
      in TrMethodSig params (if isEmpty ret then Nothing else Just ret)

    toParam p =
      let mbStr = fmap (toByteString tbls) $ spName p
          tp    = toQName tbls $ spType p
      in TrParam mbStr (if isEmpty tp then Nothing else Just tp)

toQName :: SymbolTables -> NameRef -> QName
toQName tbls ref =
    let name  = lookupName ref tbls
        mbStr = fmap (toByteString tbls) $ nmStr name
        ns    = toNs $ nmQual name

        toNs (QualNs ref) =
          let ns  = lookupNamespace ref tbls
              str = toByteString tbls $ nsName ns
         in case nsKind ns of
              NamespaceKind_General    -> QNsCustom str
              NamespaceKind_Explicit   -> QNsCustom str
              NamespaceKind_Protected  -> QNsProtected
              NamespaceKind_Private    -> QNsPrivate
              NamespaceKind_Static     -> QNsOther      -- not sure about this one
              NamespaceKind_Package    -> QNsCustom str
              NamespaceKind_Internal   -> QNsPrivate    -- treated as private
        toNs (QualNss sRef) = case nsSpaces $ lookupNameset sRef tbls of
          [nsRef]    -> toNs (QualNs nsRef)
          _          -> QNsOther                        -- complex namespace
        toNs _ = QNsOther                               -- complex namespace
    in QName ns (maybe L.empty id mbStr)

toByteString :: SymbolTables -> StringRef -> ByteString
toByteString tbls ref = L.pack $ S.encode $ lookupString ref tbls


--
-- Type hierarchies (represented as a graph)
--
-- Structure:
-- direct dependencies, indirect dependencies (transitive closure),
-- name to node map, name to rank map
--

newtype TpGr = TpGr (G.Gr QName InhInfo, G.Gr QName (), Map QName G.Node, Map QName Int)
data InhInfo = Extends | Implements
  deriving (Eq,Ord,Show)

emptyTpGr :: TpGr
emptyTpGr = TpGr (G.empty, G.empty, Map.empty, Map.empty)

instance Show TpGr where
  show (TpGr (g, _, _, _)) = G.graphviz g "types" (11.7, 8.3) (1,1) G.Landscape

-- | Creates the graph of the type hierarchy
toTypeGr :: TypeInfo -> TpGr
toTypeGr (TypeInfo mp) = TpGr (gr, G.trc gr, lab2vert, rankMp) where
  descrs = Map.elems mp
  nms    = filter isAccessible $ nub $ concat
             [ [tpName d, maybe qnObject id (tpSuper d)] ++ tpInterfaces d | d <- descrs ]
  eMap   = Map.fromListWith (++)
            [ (tpName d, maybe
                           (if tpName d == qnObject then [] else [(Extends,qnObject)])
                           (\s -> [(Extends,s)]) (tpSuper d)
                  ++ [ (Implements, s) | s <- tpInterfaces d ])
            | d <- descrs, isAccessible (tpName d) ]

  edges    = [ (a,i,b) | (a,bs) <- Map.assocs eMap, (i,b) <- bs, isAccessible a, isAccessible b ]
  lab2vert = Map.fromList $ zip nms [1..]
  vs       = zip [1..] nms
  es       = map (\(a,i,b) -> (lk a, lk b, i)) edges
  lk k     = Map.findWithDefault (error "toTypeGr: no vertex for key") k lab2vert
  gr       = G.mkGraph vs es

  ordered  = G.topsort' gr
  rankMp   = Map.fromList (zip ordered [1..])

isAccessible :: QName -> Bool
isAccessible = isNsAccessible . qNs where
  isNsAccessible (QNsCustom _) = True
  isNsAccessible _             = False

-- | Parents in sorted order
supertypes :: TpGr -> QName -> [QName]
supertypes (TpGr (_, gr, lab2vert, ranks)) root = tps where
  start  = Map.findWithDefault (error "supertypes: no vertex for key") root lab2vert
  names  = [ G.lab' (G.context gr n) | n <- G.suc gr start ]
  tps    = sortBy (\a b -> compare (rank a) (rank b)) names
  rank x = Map.findWithDefault (error "supertypes: no rank for key") x ranks

-- | Children in sorted order
subtypes :: TpGr -> QName -> [QName]
subtypes (TpGr (_, gr, lab2vert, ranks)) root = tps where
  start  = Map.findWithDefault (error "subtypes: no vertex for key") root lab2vert
  names  = [ G.lab' (G.context gr n) | n <- G.pre gr start ]
  tps    = reverse $ sortBy (\a b -> compare (rank a) (rank b)) names
  rank x = Map.findWithDefault (error "subtypes: no rank for key") x ranks

-- | Superclasses that are either subtypes or supertypes
activeTypes :: TpGr -> [QName] -> [QName]
activeTypes (TpGr (gr, _, lab2vert, _)) roots = tps where
  starts = catMaybes $ map lk roots
  lk nm  = Map.lookup nm lab2vert
  tps    = [ G.lab' (G.context gr n) | n <- G.udfs starts gr ]

-- | Requires the TpGr to be a lattice (acyclic with Object as top).
joinTypes :: TpGr -> QNameSet -> QNameSet -> QNameSet
joinTypes (TpGr (_, gr, lab2vert, _)) as bs = rs where
  av = close as
  bv = close bs
  cv = IntSet.intersection av bv
  sv = IntSet.unions [ IntSet.fromList (G.suc gr v) | v <- IntSet.toList cv ]

  rv = IntSet.difference cv sv
  rs = Set.fromList $ map (G.lab' . G.context gr) $ IntSet.toList rv

  close = IntSet.unions . map (IntSet.fromList . G.suc gr . lk) . Set.toAscList
  lk x = Map.findWithDefault (error "joinTypes: no vertex for key") x lab2vert

{-
-- | Constructs the set of effective traits for a given class or interface
effectiveTraits :: SymbolTables -> TpGr -> QName -> [(QName, TraitDescr)]
effectiveTraits tbls gr root = result where
  nodes  = parents tbls gr root
  result = nubBy cmp $ concatMap traits nodes
  cmp (_,a) (_,b) = eqTrait tbls a b

  traits n =
    let cl = findClass n tbls
        ts = maybe [] clDynTraits cl
        nm = maybe (error "effectiveTraits::traits::should not be referenced") clName cl
    in zip (repeat nm) ts
-}


--
-- Trait context
--
-- Note: traits connect CtxObjs.
-- represents the embedding of methods/classes etc.
--

data CtxTrait = CtxTrait
  { ctxTraitParent :: !CtxObj       -- type of parent
  , ctxTraitObj    :: !CtxObj       -- type of child
  , ctxTraitName   :: !QName        -- name of child
  , ctxTraitBind   :: !CtxTraitBind -- how the child is bound to the parent
  }
  deriving (Eq,Ord,Show)

data CtxObj
  = CtxObjMethod !MethodRef
  | CtxObjClass  !ClassRef
  | CtxObjGlobal
  | CtxObjScript
  deriving (Eq,Ord,Show)

data CtxTraitBind
  = BindConstructor !Bool  -- True if object constructor or otherwise False
  | BindNested
  | BindOther
  deriving (Eq,Ord,Show)

-- Note: the ctxParent component must not be made strict, because the construction
-- of this structure depends on lazy evaluation.
data CtxParents
  = CtxParentsNil
  | CtxParentsCons  { ctxTrait :: !CtxTrait, ctxParent :: CtxParents }
  deriving (Eq,Ord,Show)

-- | Extracts the method name from the parents structure
parentsToMethodName :: CtxParents -> QName
parentsToMethodName CtxParentsNil                          = qnameEmpty
parentsToMethodName (CtxParentsCons (CtxTrait _ _ nm _) _) = nm


--
-- Context information of instructions
--
data CtxInfo = CtxInfo
  { ctxTbls    :: !SymbolTables
  , ctxOpts    :: !Options
  , ctxBlock   :: !CtxBlockInfo
  , ctxMethod  :: !CtxMethodInfo
  , ctxParents :: CtxParents
  }

data CtxBlockInfo = CtxBlockInfo
  { ctxBlockId :: !Int
  }

data CtxMethodInfo = CtxMethodInfo
  { ctxMethodId :: !Word32
  }
  deriving (Show)
