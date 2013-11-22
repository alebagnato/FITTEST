module GenInh(generateInheritanceFile) where

import PrettyUtil
import ProgInfo
import Data.List(nub,intercalate)
import Text.HTML.Chunks
import System.IO
import Templates

-- TpGr is passed explicitly because the symboltables list may be empty
generateInheritanceFile :: TpGr -> [SymbolTables] -> FilePath -> IO ()
generateInheritanceFile gr tbls name = writeFile name (ppInhInfo gr tbls)

ppInhInfo :: TpGr -> [SymbolTables] -> String
ppInhInfo tpGr allTbls = format output where
  output  = Chunk_class "" imports entries
  imports = concatMap format [ Chunk_import (qnameToIdent tp) | tp <- allTypes ]
  entries = concatMap (format . mkEntry) roots

  roots    = filter isAccessible $ nub $ concatMap
               (\tbls -> map (classQName tbls) (allClasses tbls)) allTbls
  allTypes = activeTypes tpGr roots

  mkEntry nm =
    let preds = toStrSep "," $ supertypes tpGr nm
        succs = toStrSep "," $ subtypes tpGr nm
    in Chunk_entry (qnameToIdent nm) preds succs

  toStrSep s = intercalate s . map qnameToIdent

