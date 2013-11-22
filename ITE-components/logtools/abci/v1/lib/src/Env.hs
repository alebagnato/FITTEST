-- | Environments.
--   A 'dynamic' environment has a number of fixed bindings, and some new bindings.
--   The old bindings are preserved; the new ones added.
module Env(Env,StaticEnv,DynEnv,DynId,Subst,mappedTo,singleEnv,listEnv,lookupEnv,closeEnv,openEnv,mapEnv,valuesEnv,assocsEnv,keysEnv) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Word

newtype Env key elem = Env (Map key elem)
type StaticEnv elem = Env Word32 elem
type DynEnv elem    = Env DynId elem


--
-- Instances for 'Env'
--

instance (Show k, Show a) => Show (Env k a) where
  show (Env env) = show $ Map.assocs env

instance Ord k => Monoid (Env k a) where
  mempty = Env Map.empty
  mappend (Env p) (Env q) = Env (Map.union p q)


-- | A dynamic key for some value in the environment.
--   Either the key is fixed, and relates to an already existing key,
--   or it is a fresh key, which means its added to the environment.

-- Why do we need this partition:
--   To dynamically add new keys to an already filled symbol table.
--   This way it is possible to add new keys, without changing the
--   original ones.
data DynId
  = FixedId !Word32
  | FreshId !Int
  deriving (Eq,Ord,Show)


-- | Wrapper around a substitution
newtype Subst = Subst (Map Int Word32)

-- | Obtains a substitution
mappedTo :: Int -> Subst -> Word32
mappedTo key (Subst subst) = Map.findWithDefault (error ("mappedTo: no such substitution for: " ++ show key ++ " in: " ++ show subst)) key subst

-- | Wrap an item into an environment
singleEnv :: Ord k => k -> a -> Env k a
singleEnv k v = Env $ Map.singleton k v

-- | Convert a list of pairs into a static environment
listEnv :: [(Word32,a)] -> StaticEnv a
listEnv = Env . Map.fromList

-- | Obtain he pairs in the environment
assocsEnv :: Env k a -> [(k,a)]
assocsEnv (Env m) = Map.assocs m

-- | Obtain the values in the environment
valuesEnv :: Env k a -> [a]
valuesEnv (Env m) = Map.elems m

-- | Obtain the keys in the environment
keysEnv :: Env k a -> [k]
keysEnv (Env m) = Map.keys m

-- | Lookup an item in the environment (that is supposed to be there)
lookupEnv :: (Ord k, Show k, Show a) => k -> Env k a -> a
lookupEnv key e@(Env env) = Map.findWithDefault (error ("lookupEnv: no key " ++ show key ++ " in: " ++ show e)) key env

-- | Closes a dynamic environment, turning it into a static environment, and a substitution for the
--   fresh identifiers in the environment.
closeEnv :: DynEnv a -> (StaticEnv a, Subst)
closeEnv (Env env) = (Env env', Subst subst) where
  ks     = Map.keys env
  newId  = 1 + maximum (0 : [ n | (FixedId n) <- ks ])
  newKs  = [ i | (FreshId i) <- ks ]
  subst  = Map.fromList $ zip newKs [newId..]
  env'   = Map.mapKeys fKeys env
  fKeys (FreshId i) = Map.findWithDefault undefined i subst
  fkeys k = k

-- | Turns a static environment back into a dynamic environment
openEnv :: StaticEnv a -> DynEnv a
openEnv (Env env) = Env $ Map.mapKeys FixedId env

-- | Map on env
mapEnv :: (k -> a -> b) -> Env k a -> Env k b
mapEnv f (Env env) = Env $ Map.mapWithKey f env
