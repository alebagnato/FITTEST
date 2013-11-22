{-# LANGUAGE TemplateHaskell #-}
module EnvInfo where

import Data.Binary
import Data.DeriveTH
import Data.Derive.Binary
import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S
import Data.Monoid
import Data.Set(Set)
import Data.Char(isSpace)
import qualified Data.Set as Set
import System.IO
import System.Directory
import System.FilePath


--
-- Information about types in the environment
-- Note: we consider here that a type is a class.
-- There is however a difference with the ClassDescr of
-- above: that is the class description of the class in
-- the SWF file with information about how information
-- is stored, whereas here we only have the information
-- on how to access or call information on such a type.
--

data QName = QName { qNs :: !QNs, qName :: !ByteString }
  deriving (Eq,Ord)

instance Show QName where
  show = qnameToKey

-- Outputs the QName in the same representation as nameToKey
qnameToKey :: QName -> String
qnameToKey = fmtQNameWithSep ":"

qnameToIdent :: QName -> String
qnameToIdent = fmtQNameWithSep "."

fmtQNameWithSep :: String -> QName -> String
fmtQNameWithSep sep (QName (QNsCustom ns) nm) | not (L.null ns) = showByteStr ns ++ sep ++ showByteStr nm
fmtQNameWithSep _   (QName _ nm)                                = showByteStr nm

showByteStr :: ByteString -> String
showByteStr = S.decode . L.unpack

qnameEmpty :: QName
qnameEmpty = QName (QNsCustom L.empty) L.empty

type MbQName = Maybe QName
type QNames = [QName]
type QNameSet = Set QName

data QNs
  = QNsCustom { qnsName :: !ByteString }
  | QNsProtected
  | QNsPrivate
  | QNsOther
  deriving (Eq,Ord,Show)

data TypeDescr = TypeDescr
  { tpName       :: !QName             -- name of class
  , tpSuper      :: !MbQName           -- name of superclass (if any)
  , tpInterfaces :: !QNames            -- interfaces implemented by class
  , tpDynTraits  :: !TpTraitDescrs     -- dynamic traits of a class
  , tpStaTraits  :: !TpTraitDescrs     -- static traits of a class
  }
  deriving (Eq,Ord,Show)

type TpTraitDescrs = [TpTraitDescr]

data TpTraitDescr = TpTraitDescr
  { tpTrName :: !QName
  , tpTrBody :: !TpTrBody
  } deriving (Eq,Ord,Show)

data TpTrBody
  = TrMethod  { trMethodSig :: !TrMethodSig }   -- trait contains a method closure
  | TrField   { trFieldType :: !QName }         -- object contains a value of the type
  | TrClass   { trClassName :: !QName }         -- object containing static traits of a class
  deriving (Eq,Ord,Show)

data TrMethodSig = TrMethodSig
  { trSigParams :: !TrParams    -- has zero or more parameters, with maybe a name
  , trSigReturn :: !MbQName     -- maybe a type (otherwise '*')
  } deriving (Eq,Ord,Show)

type TrParams = [TrParam]
data TrParam = TrParam
  { trParamName :: !(Maybe ByteString)   -- maybe a name of a parameter (likely no)t
  , trParamType :: !MbQName              -- maybe a type (otherwise '*')
  }
  deriving (Eq,Ord,Show)

-- such a map stores information about types
newtype TypeInfo = TypeInfo (Map QName TypeDescr)

-- Automatically derive binary instances
$( derives [makeBinary]
     [ ''QName, ''QNs, ''TypeDescr, ''TpTraitDescr, ''TpTrBody
     , ''TrMethodSig, ''TrParam, ''TypeInfo] )

anyTypeDescr = TypeDescr
  { tpName       = QName QNsOther L.empty
  , tpSuper      = Nothing
  , tpInterfaces = []
  , tpDynTraits  = []
  , tpStaTraits  = []
  }

qnObject :: QName
qnObject = QName (QNsCustom L.empty) (L.pack $ S.encode "Object")

lookupType :: TypeInfo -> QName -> TypeDescr
lookupType (TypeInfo mp) nm = Map.findWithDefault anyTypeDescr nm mp

containsType :: TypeInfo -> QName -> Bool
containsType (TypeInfo mp) nm = Map.member nm mp

allTypes :: TypeInfo -> Set QName
allTypes (TypeInfo mp) = Map.keysSet mp

isEmpty :: QName -> Bool
isEmpty = L.null . qName

instance Show TypeInfo where
  show (TypeInfo mp) = "ENTRIES\r\n" ++ show mp

instance Monoid TypeInfo where
  mempty = TypeInfo Map.empty
  (TypeInfo m1) `mappend` (TypeInfo m2) = TypeInfo (Map.union m1 m2)

loadTypeInfo :: FilePath -> IO TypeInfo
loadTypeInfo file = decode `fmap` L.readFile file

saveTypeInfo :: TypeInfo -> FilePath -> IO ()
saveTypeInfo info file =
  let file' | hasExtension file = file
            | otherwise         = addExtension file typeInfoExt
  in L.writeFile file' (encode info)

typeInfoExt :: String
typeInfoExt = ".env"

loadTypeInfos :: FilePath -> IO TypeInfo
loadTypeInfos path = do
  isDir <- doesDirectoryExist path
  if isDir
   then loadDirTypeInfo typeInfoExt path
   else do isFile <- doesFileExist path
           if isFile
            then loadTypeInfo path
            else do hPutStrLn stderr ("loadTypeInfos: skipped loading non-exsiting path: " ++ path)
                    return mempty

-- | Loads all .ext files from directory dir
loadDirTypeInfo :: String -> FilePath -> IO TypeInfo
loadDirTypeInfo ext dir = do
  paths <- getDirectoryContents dir
  let envFiles = [ combine dir p | p <- paths, takeExtension p == typeInfoExt ]
  infos <- mapM loadTypeInfo envFiles
  return $ mconcat infos
