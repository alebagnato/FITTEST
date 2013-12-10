-- | Extracts symbolic information from a generated Haskell file
module SymInfo(SymInfo(..),extractSymInfo,extractSymInfos) where

import qualified Data.Set as Set
import Data.Set(Set)
import Data.Monoid
import Language.Haskell.Exts
import System.IO
import System.IO.Error


-- | Carries symbolic information around.
data SymInfo = SymInfo { allTypes :: !(Set String), allFields :: !(Set String), allModules :: !(Set String) }
  deriving (Eq,Ord,Show)

-- | Monoid instance allows merging of sym info structures.
instance Monoid SymInfo where
  mempty = SymInfo mempty mempty mempty
  (SymInfo t1 f1 m1) `mappend` (SymInfo t2 f2 m2) = SymInfo (t1 `mappend` t2) (f1 `mappend` f2) (m1 `mappend` m2)


-- | Extracts symbolic info from multiple files
extractSymInfos :: [FilePath] -> IO SymInfo
extractSymInfos paths = do
  infos <- mapM extractSymInfo paths
  return $ foldr mappend mempty infos


-- | Runs the parser.
extractSymInfo :: FilePath -> IO SymInfo
extractSymInfo path = do
  str <- readFile path
  case parseModuleWithMode (defaultParseMode { parseFilename = path, ignoreLanguagePragmas = False, extensions = exts }) str of
    ParseFailed loc msg -> ioError $ userError ("Parse failed at " ++ show loc ++ ": " ++ msg)
    ParseOk m -> return $ toSymInfo m
  where exts = [ OverlappingInstances,MultiParamTypeClasses,FunctionalDependencies
               , FlexibleContexts,FlexibleInstances,EmptyDataDecls,TypeSynonymInstances ]

-- | Takes a Haskell module and returns the symbolic information contained in it.
toSymInfo :: Module -> SymInfo
toSymInfo (Module _ (ModuleName nm) _ _ _ _ decls) = SymInfo tps flds nms where
  nms  = Set.singleton nm
  tps  = strs "TypeObject"
  flds = strs "Key"
  strs nm = foldr mappend mempty (map (allStrs nm) decls)

allStrs :: String -> Decl -> Set String
allStrs nm (PatBind _ _ _ (UnGuardedRhs e) _) = case e of
  App (Con (UnQual (Ident x))) (Lit (String s)) | x == nm -> Set.singleton s
  _        -> mempty
allStrs _ _ = mempty
