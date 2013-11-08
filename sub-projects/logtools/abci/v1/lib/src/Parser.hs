-- | Parser for ActionScript byte code.
--
-- Some parts of the grammar are not documented in the AVM2 specification.
-- In particular:
-- * generics: http://www.n-heptane.com/nhlab/repos/haskell-swf/
--
-- Note: the parser inserts labels to each instruction
--       all jumps use absolute addresses to labels
-- Note: we insert extra virtual instructions, and number these as
--       a post-processing step.
--
-- Check-out the various grammar definitions of .swf, .swc, .abc for
-- background info about the structure of the parser as specified here.
--
-- The parsers are annotated with debugging information, which can be
-- used to quickly track-down issues in the parser. So far, the parsers
-- have been tested with an extensive test-set from an avm interpreter,
-- and also have been verified to parse successfully all Flash components
-- shipped with version 4 of Flex and version 10 of the Flash player.
--

module Parser(parseAbcFile,parseSwfFile,parseAbcAsSwf,parseAnyAsSwf) where

import Data.Bits
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8(fromString)
import Data.Word
import System.IO
import ParseUtil
import ByteCode
import InsertLabels
import Codec.Archive.Zip
import System.FilePath
import BranchRelToAbs
import FixJumpDestinations
import RenumberBlocks


--
-- SWF parsing
--

-- Note that the SwfFile may be compressed. In that case, we
-- first decompress it and then continue parsing on the
-- decompressed stream. This may cause the original string to
-- be retained in memory until the parsing is finished: it's
-- not entirely clear if the lazy bytestrings that we use
-- ensure that this is not an issue. So far, this has not become
-- a big problem.
pSwfFile :: Parser SwfFile
pSwfFile = do
  compressed <- pSwfHead
  version    <- pU8
  length     <- pW32

      -- the check for end is important in order to find out if we
      -- really parsed the entire file and did not accidentally stop
      -- somewhere in the middle. This check should not fail, because
      -- the grammar effectively does not have alternatives.
  let checkFinished p = p <* pEnd

      -- runs the parser on the remainder after parsing the header
      pTail | compressed = pDecompress length . checkFinished
            | otherwise  = checkFinished
  pTail $ do
    size  <- padded pRect
    rate  <- pU16
    count <- pU16
    tags  <- pTags
    return $ renumberBlocks $ (SwfFile_File compressed version length size rate count tags)

pSwfHead :: Parser Bool
pSwfHead =
  pTable [ 0x46 .> return False
         , 0x43 .> return True ]
  <* pByte 0x57
  <* pByte 0x53
  <?> "SWF head"

-- a rectangle is a variable sized bit-structure. It is not
-- necessarily byte-aligned (depending on its context). The
-- Bit-monad allows parsers for these bit structures to be
-- composed and a special combinator runs the bit-parser on
-- the actual byte stream (and deals with padding).
pRect :: Bit Rect
pRect = do
  size <- fromIntegral <$> takeBits 5
  x1 <- takeBits size
  y1 <- takeBits size
  x2 <- takeBits size
  y2 <- takeBits size
  return (Rect_Rect size x1 x2 y1 y2)

pTags :: Parser Tags
pTags = do
  t <- pTag
  case t of
    Tag_End -> return [t]
    _       -> do ts <- pTags
                  return (t : ts)

pTag :: Parser Tag
pTag = do
    key <- pU16
    let code = shiftR key 6
        len' = key .&. 0x3F
    len <- if len' == 0x3F
           then pW32
           else return (fromIntegral len')
    let kind = toTagKind code
    pInfo ("Tag: " ++ show kind ++ ", len: " ++ show len)
    case kind of
      TagKind_DoABC -> Tag_Abc <$> pAbcFlags <*> pNullString <*> pAbcFile
      TagKind_End   -> return Tag_End
      TagKind_FileAttributes
                    -> padded $ do
                         takeBit
                         direct <- takeBit
                         gpu    <- takeBit
                         meta   <- takeBit
                         as3    <- takeBit
                         takeBits 2
                         net    <- takeBit
                         takeBits 24
                         return $ Tag_FileAttributes direct gpu meta as3 net
      _             -> Tag_Opaque kind len . B.fromChunks . return <$> getBytes (fromIntegral len)
  <?> "tag"

pAbcFlags :: Parser AbcFlags
pAbcFlags = do
  n <- pW32
  case n of
    0 -> return []
    1 -> return [AbcFlag_LazyInit]
    _ -> fail ("Unknown ABC flags: " ++ show n)

toTagKind :: Word16 -> TagKind
toTagKind c = case c of
  0  -> TagKind_End
  1  -> TagKind_ShowFrame
  2  -> TagKind_DefineShape
  4  -> TagKind_PlaceObject
  5  -> TagKind_RemoveObject
  6  -> TagKind_DefineBits
  7  -> TagKind_DefineButton
  8  -> TagKind_JPEGTables
  9  -> TagKind_SetBackgroundColor
  10 -> TagKind_DefineFont
  11 -> TagKind_DefineText
  12 -> TagKind_DoAction
  13 -> TagKind_DefineFontInfo
  14 -> TagKind_DefineSound
  15 -> TagKind_StartSound
  17 -> TagKind_DefineButtonSound
  18 -> TagKind_SoundStreamHead
  19 -> TagKind_SoundStreamBlock
  20 -> TagKind_DefineBitsLossless
  21 -> TagKind_DefineBitsJPEG2
  22 -> TagKind_DefineShape2
  23 -> TagKind_DefineButtonCxform
  24 -> TagKind_Protect
  26 -> TagKind_PlaceObject2
  28 -> TagKind_RemoveObject2
  32 -> TagKind_DefineShape3
  33 -> TagKind_DefineText2
  34 -> TagKind_DefineButton2
  35 -> TagKind_DefineBitsJPEG3
  36 -> TagKind_DefineBitsLossless2
  37 -> TagKind_DefineEditText
  39 -> TagKind_DefineSprite
  43 -> TagKind_FrameLabel
  45 -> TagKind_SoundStreamHead2
  46 -> TagKind_DefineMorphShape
  48 -> TagKind_DefineFont2
  56 -> TagKind_ExportAssets
  57 -> TagKind_ImportAssets
  58 -> TagKind_EnableDebugger
  59 -> TagKind_DoInitAction
  60 -> TagKind_DefineVideoStream
  61 -> TagKind_VideoFrame
  62 -> TagKind_DefineFontInfo2
  64 -> TagKind_EnableDebugger2
  65 -> TagKind_ScriptLimits
  66 -> TagKind_SetTabIndex
  69 -> TagKind_FileAttributes
  70 -> TagKind_PlaceObject3
  71 -> TagKind_ImportAssets2
  73 -> TagKind_DefineFontAlignZones
  74 -> TagKind_CSMTextSettings
  75 -> TagKind_DefineFont3
  76 -> TagKind_SymbolClass
  77 -> TagKind_Metadata
  78 -> TagKind_DefineScalingGrid
  82 -> TagKind_DoABC
  83 -> TagKind_DefineShape4
  84 -> TagKind_DefineMorphShape2
  86 -> TagKind_DefineSceneAndFrameLabelData
  87 -> TagKind_DefineBinaryData
  88 -> TagKind_DefineFontName
  89 -> TagKind_StartSound2
  90 -> TagKind_DefineBitsJPEG4
  91 -> TagKind_DefineFont4
  _  -> TagKind_Other c  -- fallback case


--
-- ABC parsing
--

pAbcFile :: Parser AbcFile
pAbcFile = do
    major   <- pU16 <?> "major version"
    minor   <- pU16 <?> "minor version"
    pool    <- pPoolInfo
    methods <- pCountU30 pMethodInfo
    metas   <- pCountU30 pMetaInfo
    classCount <- fromIntegral <$> pU30
    instances  <- pCount pInstanceInfo classCount
    classes    <- pCount pClassInfo classCount
    scripts <- pCountU30 pScriptInfo
    bodies  <- pCountU30 pBodyInfo
    return (AbcFile_File major minor pool methods metas instances classes scripts bodies)
  <?> "abc file"

pPoolInfo :: Parser PoolInfo
pPoolInfo = PoolInfo_Info
  <$> pCount1U30 pS32
  <*> pCount1U30 pU32
  <*> pCount1U30 pD64
  <*> pCount1U30 pString
  <*> pCount1U30 pNamespaceInfo
  <*> pCount1U30 pSetInfo
  <*> pCount1U30 pMultinameInfo
  <?> "pool info"

pNamespaceInfo :: Parser NamespaceInfo
pNamespaceInfo = NamespaceInfo_Info
  <$> pNamespaceKind
  <*> pNamespaceIndex
  <?> "namespace info"

pNamespaceKind :: Parser NamespaceKind
pNamespaceKind = pTable
  [ 0x08 .> return NamespaceKind_General
  , 0x16 .> return NamespaceKind_Package
  , 0x17 .> return NamespaceKind_Internal
  , 0x18 .> return NamespaceKind_Protected
  , 0x19 .> return NamespaceKind_Explicit
  , 0x1A .> return NamespaceKind_Static
  , 0x05 .> return NamespaceKind_Private
  ] <?> "namespace kind"

pNamespaceIndex :: Parser Word32
pNamespaceIndex = pU30 <?> "namespace name index"

pSetInfo :: Parser SetInfo
pSetInfo = SetInfo_Info <$> pCountU30 pU30 <?> "namespace set info"

pMultinameInfo :: Parser MultinameInfo
pMultinameInfo = pTable
  [ 0x07 .> MultinameInfo_QName       <$> pNamespaceIndex <*> pNameIndex
  , 0x0D .> MultinameInfo_QNameA      <$> pNamespaceIndex <*> pNameIndex
  , 0x0F .> MultinameInfo_RTQName     <$> pNameIndex
  , 0x10 .> MultinameInfo_RTQNameA    <$> pNameIndex
  , 0x11 .> return MultinameInfo_RTQNameL
  , 0x12 .> return MultinameInfo_RTQNameLA
  , 0x09 .> MultinameInfo_Multiname   <$> pNameIndex <*> pSetIndex
  , 0x0E .> MultinameInfo_MultinameA  <$> pNameIndex <*> pSetIndex
  , 0x1B .> MultinameInfo_MultinameL  <$> pSetIndex
  , 0x1C .> MultinameInfo_MultinameLA <$> pSetIndex
  , 0x1D .> MultinameInfo_Generic     <$> pNameIndex <*> pCountU30 pNameIndex
  ] <?> "multiname info"

pNameIndex :: Parser Word32
pNameIndex = pU30 <?> "name index"

pSetIndex :: Parser Word32
pSetIndex = pU30 <?> "set index"

pMethodInfo :: Parser MethodInfo
pMethodInfo = do
  nParams    <- pLength
  rt         <- pMultinameIndex
  paramTps   <- pCount pMultinameIndex nParams
  nm         <- pNameIndex
  flags      <- pMethodFlags
  options    <- if MethodFlag_HasOptionals `elem` flags
                then pCountU30 pOptional
                else return []
  paramNames <- if MethodFlag_HasParamNames `elem` flags
                then pCount pNameIndex nParams
                else return []
  return (MethodInfo_Info rt paramTps nm flags options paramNames)
 <?> "method info"

pLength :: Parser Int
pLength = fromIntegral <$> pU30 <?> "length"

pMultinameIndex :: Parser Word32
pMultinameIndex = pU30 <?> "multiname index"

pMethodFlags :: Parser MethodFlags
pMethodFlags = pFlags [ (0x01, MethodFlag_NeedArgs)      -- the method gets parameters delivered in an array instead of registers
                      , (0x02, MethodFlag_NeedAct)       -- upon execution, the method uses an explicit activation object storing local variables
                      , (0x04, MethodFlag_NeedRest)
                      , (0x08, MethodFlag_HasOptionals)  -- a bit of a cryptic name...
                      , (0x40, MethodFlag_SetDXNS)
                      , (0x80, MethodFlag_HasParamNames)
                      ]

pFlags :: [(Word8,a)] -> Parser [a]
pFlags flags = matchFlags flags <$> pU8 <?> "flags"

-- | takes those flags that have corresponding bits in the word
matchFlags :: [(Word8,a)] -> Word8 -> [a]
matchFlags flags b = map snd $ filter (\(x,_) -> x .&. b > 0) flags

pOptional :: Parser Optional
pOptional = Optional_Detail
  <$> pU32
  <*> pValueKind
  <?> "optional"

pValueKind :: Parser ValueKind
pValueKind = pTable
  [ 0x03 .> return ValueKind_Int
  , 0x04 .> return ValueKind_UInt
  , 0x06 .> return ValueKind_Double
  , 0x01 .> return ValueKind_Utf8
  , 0x0B .> return ValueKind_True
  , 0x0A .> return ValueKind_False
  , 0x0C .> return ValueKind_Null
  , 0x00 .> return ValueKind_Undefined
  , 0x08 .> return ValueKind_Namespace
  , 0x16 .> return ValueKind_Package
  , 0x17 .> return ValueKind_Internal
  , 0x18 .> return ValueKind_Protected
  , 0x19 .> return ValueKind_Explicit
  , 0x1A .> return ValueKind_Static
  , 0x05 .> return ValueKind_Private
  ] <?> "constant kind"

-- we probably never really have to know what is contained in these meta infos.
pMetaInfo :: Parser MetaInfo
pMetaInfo = MetaInfo_Info <$> pNameIndex <*> pCountU30 pMetaItem <?> "meta info"

pMetaItem :: Parser MetaItem
pMetaItem = MetaItem_Item <$> pNameIndex <*> pNameIndex <?> "meta item"

-- Description of the structure of objects of a certain class
pInstanceInfo :: Parser InstanceInfo
pInstanceInfo = do
    name  <- pMultinameIndex
    super <- pMultinameIndex
    flags <- pFlags [ (0x01, InstanceFlag_ClassSealed)
                    , (0x02, InstanceFlag_ClassFinal)
                    , (0x04, InstanceFlag_ClassInterface)
                    , (0x08, InstanceFlag_ClassProtected)
                    ]
    protected <- if InstanceFlag_ClassProtected `elem` flags
                 then (pNamespaceIndex <?> "protected")
                 else return 0
    itfs   <- pCountU30 pMultinameIndex
    con    <- pMethodIndex
    traits <- pCountU30 pTrait
    return (InstanceInfo_Info name super flags protected itfs con traits)
  <?> "instance info"

pMethodIndex :: Parser Word32
pMethodIndex = pU30 <?> "method index"

pTrait :: Parser Trait
pTrait = do
    name <- pMultinameIndex
    info <- pU8
    pInfo ("trait info: " ++ show info)
    let tp = info .&. 0x0F
    body <- case tp of
              0 -> pTraitSlot TraitData_Slot
              1 -> pTraitMethod TraitData_Method
              2 -> pTraitMethod TraitData_Getter
              3 -> pTraitMethod TraitData_Setter
              4 -> pTraitClass
              5 -> pTraitFun
              6 -> pTraitSlot TraitData_Const
              _ -> fail ("unknown trait type: " ++ show info)
    let flags = shiftR info 4
        attrs = matchFlags [ (0x1, TraitAttr_Final)
                           , (0x2, TraitAttr_Override)
                           , (0x4, TraitAttr_Metadata)
                           ] flags
    meta <- if TraitAttr_Metadata `elem` attrs
            then pCountU30 pMetadataIndex
            else return []
    return (Trait_Trait name body attrs meta)
  <?> "trait"

-- provides the common parsing of various slot-based traits.
-- it may actually be a good idea to refactor this a bit and
-- make this common structure visible as separate nodes in the AST.
pTraitSlot :: (Word32 -> Word32 -> Word32 -> ValueKind -> TraitData) -> Parser TraitData
pTraitSlot f = do
    slotId   <- pSlotId
    typeName <- pMultinameIndex
    vindex   <- (pU30 <?> "vindex")
    vkind    <- if vindex > 0
                then pValueKind
                else return ValueKind_Undefined
    return (f slotId typeName vindex vkind)
  <?> "trait slot"

pTraitMethod :: (Word32 -> Word32 -> TraitData) -> Parser TraitData
pTraitMethod f = f <$> pDispId <*> pMethodIndex <?> "trait method"

pTraitClass :: Parser TraitData
pTraitClass = TraitData_Class <$> pSlotId <*> pClassIndex <?> "trait class"

pTraitFun :: Parser TraitData
pTraitFun = TraitData_Function <$> pSlotId <*> pMethodIndex <?> "trait fun"

pDispId :: Parser Word32
pDispId = pU30 <?> "dispatch id"

pSlotId :: Parser Word32
pSlotId = pU30 <?> "slot id"

pClassIndex :: Parser Word32
pClassIndex = pU30 <?> "class index"

pMetadataIndex :: Parser Word32
pMetadataIndex = pU30 <?> "metadata index"

pClassInfo :: Parser ClassInfo
pClassInfo = ClassInfo_Info <$> pMethodIndex <*> pCountU30 pTrait <?> "class info"

pScriptInfo :: Parser ScriptInfo
pScriptInfo = ScriptInfo_Info <$> pMethodIndex <*> pCountU30 pTrait <?> "script info"

-- | Parses a method body and then replaces the offsets of all instructions
--   with labels. Also the jumps are changed so that they point to these
--   labels. Note that this requires the jumps to be pointing to the beginning
--   of an instruction. This is done by fixJumps.
pBodyInfo :: Parser BodyInfo
pBodyInfo = (insertLabels . fixJumps) <$> pBodyInfo'

-- | Parses a method body with an abundance of location information
pBodyInfo' :: Parser BodyInfo
pBodyInfo' = BodyInfo_Info
  <$> pMethodIndex
  <*> (pU30 <?> "max stack")
  <*> (decr <$> pU30 <?> "local count")
  <*> (pU30 <?> "init scope depth")
  <*> (pU30 <?> "max scope depth")
  <*> (InstructionsTop_Top <$> pInstrs)
  <*> pCountU30 pException
  <*> pCountU30 pTrait
  <?> "method body"

-- non-overloaded "-1".
decr :: Word32 -> Word32
decr x = x - 1

pInstrs :: Parser [LabInstruction]
pInstrs = do
  n <- pU30
  p <- pPos
  pInstrArray p (p + fromIntegral n)

-- | Parses the next instruction, if its location is before the 'endPos'
pInstrArray :: Int -> Int -> Parser [LabInstruction]
pInstrArray beginPos endPos = rec where
  rec = do p  <- pPos
           let offset = p - beginPos
           if p < endPos
            then do hd <- pInstr
                    fmap ((branchRelToAbs $ LabInstruction_Instr offset hd) :) rec
            else return [LabInstruction_Instr offset $ Instruction_Virtual 0 $ VirtKind_Terminator]

pInstr :: Parser Instruction
pInstr = pTable table <?> "instruction"

-- parsing of various instructions. The parsing fails if an opcode is read
-- that is not in the list. This may then either be some new instruction, or
-- some bytes before this instruction were interpreted wrongly.
-- In our experience, when there is a parse error, then the cause of the
-- error is usually quite close to the location reported by the parser.
table :: [(Word8, Parser Instruction)]
table =
  [ 0xA0 .> return Instruction_Add
  , 0xC5 .> return Instruction_Add_i
  , 0x9B .> return Instruction_Add_d
  , 0x53 .> Instruction_ApplyType <$> pU30 -- parameter count
  , 0x86 .> Instruction_AsType <$> pMultinameIndex
  , 0x87 .> return Instruction_AsTypeLate
  , 0xA8 .> return Instruction_BitAnd
  , 0x97 .> return Instruction_BitNot
  , 0xA9 .> return Instruction_BitOr
  , 0xAA .> return Instruction_BitXor
  , 0x01 .> return Instruction_Breakpoint
  , 0xF2 .> Instruction_BreakLine <$> pLine
  , 0x41 .> Instruction_Call <$> pArgCount
  , 0x4D .> Instruction_CallInterface  <$> pNameIndex   <*> pArgCount
  , 0x43 .> Instruction_CallMethod     <$> pIndex       <*> pArgCount
  , 0x46 .> Instruction_CallProp       <$> pNameIndex   <*> pArgCount
  , 0x4C .> Instruction_CallPropLex    <$> pNameIndex   <*> pArgCount
  , 0x4F .> Instruction_CallPropVoid   <$> pNameIndex   <*> pArgCount
  , 0x44 .> Instruction_CallStatic     <$> pMethodIndex <*> pArgCount
  , 0x45 .> Instruction_CallSuper      <$> pNameIndex   <*> pArgCount
  , 0x4B .> return Instruction_CallSuperId
  , 0x4E .> Instruction_CallSuperVoid  <$> pNameIndex   <*> pArgCount
  , 0x78 .> return Instruction_CheckFilter
  , 0x80 .> Instruction_Coerce         <$> pNameIndex
  , 0x81 .> return Instruction_Coerce_b
  , 0x82 .> return Instruction_Coerce_a
  , 0x83 .> return Instruction_Coerce_i
  , 0x84 .> return Instruction_Coerce_d
  , 0x85 .> return Instruction_Coerce_s
  , 0x88 .> return Instruction_Coerce_u
  , 0x89 .> return Instruction_Coerce_o
  , 0x9A .> return Instruction_Concat
  , 0x42 .> Instruction_Construct      <$> pArgCount
  , 0x4A .> Instruction_ConstructProp  <$> pNameIndex <*> pArgCount
  , 0x49 .> Instruction_ConstructSuper <$> pArgCount
  , 0x76 .> return Instruction_Convert_b
  , 0x73 .> return Instruction_Convert_i
  , 0x75 .> return Instruction_Convert_d
  , 0x77 .> return Instruction_Convert_o
  , 0x74 .> return Instruction_Convert_u
  , 0x70 .> return Instruction_Convert_s
  , 0xEF .> Instruction_Debug          <$> pDebugType <*> pNameIndex <*> pU8 <*> pU30
  , 0xF1 .> Instruction_DebugFile      <$> pNameIndex
  , 0xF0 .> Instruction_DebugLine      <$> pLine
  , 0x94 .> Instruction_DecLocal       <$> pRegIndex
  , 0xC3 .> Instruction_DecLocal_i     <$> pRegIndex
  , 0x93 .> return Instruction_Decrement
  , 0xC1 .> return Instruction_Decrement_i
  , 0x6A .> Instruction_DeleteProperty <$> pNameIndex
  , 0x6B .> return Instruction_DeletePropertyLate
  , 0xA3 .> return Instruction_Divide
  , 0x2A .> return Instruction_Dup
  , 0x06 .> Instruction_Dxns           <$> pNameIndex
  , 0x07 .> return Instruction_DxnsLate
  , 0xAB .> return Instruction_Equals
  , 0x72 .> return Instruction_EscXAttr
  , 0x71 .> return Instruction_EscXElem
  , 0x5F .> Instruction_FindDef <$> pNameIndex
  , 0x5B .> Instruction_FindPropertyGlobalStrict <$> pNameIndex
  , 0x5C .> Instruction_FindPropertyGlobal <$> pNameIndex
  , 0x5E .> Instruction_FindProperty   <$> pNameIndex
  , 0x5D .> Instruction_FindPropStrict <$> pNameIndex
  , 0x59 .> Instruction_GetDescendants <$> pNameIndex
  , 0x64 .> return Instruction_GetGlobalScope
  , 0x6E .> Instruction_GetGlobalSlot  <$> pSlotIndex
  , 0x60 .> Instruction_GetLex         <$> pNameIndex
  , 0x62 .> Instruction_GetLocal       <$> pRegIndex
  , 0xD0 .> return Instruction_GetLocal0
  , 0xD1 .> return Instruction_GetLocal1
  , 0xD2 .> return Instruction_GetLocal2
  , 0xD3 .> return Instruction_GetLocal3
  , 0x67 .> Instruction_GetOuterScope  <$> pNameIndex
  , 0x66 .> Instruction_GetProperty    <$> pNameIndex
  , 0x65 .> Instruction_GetScopeObject <$> (pU8 <?> "scope index")
  , 0x6C .> Instruction_GetSlot        <$> pSlotIndex
  , 0x04 .> Instruction_GetSuper       <$> pNameIndex
  , 0xB0 .> return Instruction_GreaterEquals
  , 0xAF .> return Instruction_GreaterThan
  , 0x1F .> return Instruction_HasNext
  , 0x32 .> Instruction_HasNext2       <$> pRegIndex <*> pRegIndex
  , 0x13 .> Instruction_IfEq           <$> pOffset
  , 0x12 .> Instruction_IfFalse        <$> pOffset
  , 0x18 .> Instruction_IfGe           <$> pOffset
  , 0x17 .> Instruction_IfGt           <$> pOffset
  , 0x16 .> Instruction_IfLe           <$> pOffset
  , 0x15 .> Instruction_IfLt           <$> pOffset
  , 0x0F .> Instruction_IfNGe          <$> pOffset
  , 0x0E .> Instruction_IfNGt          <$> pOffset
  , 0x0D .> Instruction_IfNLe          <$> pOffset
  , 0x0C .> Instruction_IfNLt          <$> pOffset
  , 0x14 .> Instruction_IfNe           <$> pOffset
  , 0x19 .> Instruction_IfStrictEq     <$> pOffset
  , 0x1A .> Instruction_IfStrictNe     <$> pOffset
  , 0x11 .> Instruction_IfTrue         <$> pOffset
  , 0xB4 .> return Instruction_In
  , 0x92 .> Instruction_IncLocal       <$> pRegIndex
  , 0xC2 .> Instruction_IncLocal_i     <$> pRegIndex
  , 0x91 .> return Instruction_Increment
  , 0xC0 .> return Instruction_Increment_i
  , 0x68 .> Instruction_InitProperty   <$> pNameIndex
  , 0xB1 .> return Instruction_InstanceOf
  , 0xB2 .> Instruction_IsType         <$> pNameIndex
  , 0xB3 .> return Instruction_IsTypeLate
  , 0x10 .> Instruction_Jump           <$> pOffset
  , 0x08 .> Instruction_Kill           <$> pRegIndex
  , 0x09 .> return Instruction_Label
  , 0xAE .> return Instruction_LessEquals
  , 0xAD .> return Instruction_LessThan
  , 0x38 .> return Instruction_LoadFloat32
  , 0x39 .> return Instruction_LoadFloat64
  , 0x35 .> return Instruction_LoadIndirect8
  , 0x36 .> return Instruction_LoadIndirect16
  , 0x37 .> return Instruction_LoadIndirect32
  , 0x1B .> Instruction_LookupSwitch   <$> pOffset <*> (pU30 >>= pCount pOffset . (+1) . fromIntegral)
  , 0xA5 .> return Instruction_Lshift
  , 0xA4 .> return Instruction_Modulo
  , 0xA2 .> return Instruction_Multiply
  , 0xC7 .> return Instruction_Multiply_i
  , 0x90 .> return Instruction_Negate
  , 0xC4 .> return Instruction_Negate_i
  , 0x57 .> return Instruction_NewActivation
  , 0x56 .> Instruction_NewArray <$> pArgCount
  , 0x5A .> Instruction_NewCatch <$> pExceptionIndex
  , 0x58 .> Instruction_NewClass <$> pClassIndex
  , 0x40 .> Instruction_NewFunction <$> pMethodIndex
  , 0x55 .> Instruction_NewObject <$>pArgCount
  , 0x1E .> return Instruction_NextName
  , 0x23 .> return Instruction_NextValue
  , 0x02 .> return Instruction_Nop
  , 0x96 .> return Instruction_Not
  , 0x29 .> return Instruction_Pop
  , 0x1D .> return Instruction_PopScope
  , 0x24 .> Instruction_PushByte       <$> pU8
  , 0x2F .> Instruction_PushDouble     <$> pDoubleIndex
  , 0x27 .> return Instruction_PushFalse
  , 0x2D .> Instruction_PushInt        <$> pU30
  , 0x31 .> Instruction_PushNamespace  <$> pNamespaceIndex
  , 0x28 .> return Instruction_PushNaN
  , 0x20 .> return Instruction_PushNull
  , 0x30 .> return Instruction_PushScope
  , 0x25 .> Instruction_PushShort      <$> pU30
  , 0x2C .> Instruction_PushString     <$> pStringIndex
  , 0x26 .> return Instruction_PushTrue
  , 0x2E .> Instruction_PushUInt       <$> pIntIndex
  , 0x21 .> return Instruction_PushUndefined
  , 0x1C .> return Instruction_PushWith
  , 0x48 .> return Instruction_ReturnValue
  , 0x47 .> return Instruction_ReturnVoid
  , 0xA6 .> return Instruction_Rshift
  , 0x63 .> Instruction_SetLocal       <$> pRegIndex
  , 0xD4 .> return Instruction_SetLocal0
  , 0xD5 .> return Instruction_SetLocal1
  , 0xD6 .> return Instruction_SetLocal2
  , 0xD7 .> return Instruction_SetLocal3
  , 0x6F .> Instruction_SetGlobalSlot  <$> pSlotIndex
  , 0x61 .> Instruction_SetProperty    <$> pNameIndex
  , 0x69 .> return Instruction_SetPropertyLate
  , 0x6D .> Instruction_SetSlot        <$> pSlotIndex
  , 0x05 .> Instruction_SetSuper       <$> pNameIndex
  , 0x50 .> return Instruction_SignExtend1
  , 0x51 .> return Instruction_SignExtend8
  , 0x52 .> return Instruction_SignExtend16
  , 0x3D .> return Instruction_StoreFloat32
  , 0x3E .> return Instruction_StoreFloat64
  , 0x3A .> return Instruction_StoreIndirect8
  , 0x3B .> return Instruction_StoreIndirect16
  , 0x3C .> return Instruction_StoreIndirect32
  , 0xAC .> return Instruction_StrictEquals
  , 0xA1 .> return Instruction_Substract
  , 0xC6 .> return Instruction_Substract_i
  , 0x2B .> return Instruction_Swap
  , 0x03 .> return Instruction_Throw
  , 0xF3 .> return Instruction_Timestamp
  , 0x95 .> return Instruction_TypeOf
  , 0xA7 .> return Instruction_Urshift
  ]

infix 2 .>
(.>) :: Word8 -> Parser a -> (Word8, Parser a)
k .> v = (k, v)

pArgCount :: Parser Word32
pArgCount = pU30 <?> "argument count"

pIndex :: Parser Word32
pIndex = pU30 <?> "index"

pSlotIndex :: Parser Word32
pSlotIndex = pU30 <?> "slot"

pRegIndex :: Parser Word32
pRegIndex = pU30 <?> "register"

pOffset :: Parser Word32
pOffset = pS24 <?> "offset"

pLine :: Parser Word32
pLine = pU30 <?> "line nr"

pExceptionIndex :: Parser Word32
pExceptionIndex = pU30 <?> "exception index"

pIntIndex :: Parser Word32
pIntIndex = pU30 <?> "int index"

pDoubleIndex :: Parser Word32
pDoubleIndex = pU30 <?> "double index"

pStringIndex :: Parser Word32
pStringIndex = pU30 <?> "string index"

pDebugType :: Parser DebugType
pDebugType =
      DebugType_Local <$ pByte 0x01
  <?> "debug type"

pException :: Parser Exception
pException = Exception_Info
  <$> (pU30 <?> "from index")
  <*> (pU30 <?> "to index")
  <*> (pU30 <?> "target index")
  <*> pNameIndex
  <*> pNameIndex
  <?> "exception"

parseAbcFile :: FilePath -> IO AbcFile
parseAbcFile = parseIt (pAbcFile <* pEnd)

parseSwfFile :: FilePath -> IO SwfFile
parseSwfFile = parseIt pSwfFile

parseAbcAsSwf :: FilePath -> IO SwfFile
parseAbcAsSwf = parseIt (abcToSwf <$> pAbcFile <* pEnd)

-- Wraps an .abc module as an .swf module
abcToSwf :: AbcFile -> SwfFile
abcToSwf abc = wrapTagsAsSwf [code] where
  code  = Tag_Abc [] (fromString "abc") abc

-- tries to discover which kind of flash file is in the
-- file pointed to by FilePath. If it is a .swc component,
-- it uses a pkzip library to extract the .swf file in
-- the .swc component, and then parsers that file.
parseAnyAsSwf :: FilePath -> IO SwfFile
parseAnyAsSwf = parseAny
  [ \str -> if B.take 2 str == hPkZip
            then Just $! decompress str
            else Nothing
  , \str -> let hd = B.take 3 str
            in if hd == hUSwf || hd == hCSwf
               then Just $! runParser pSwfFile str
               else Nothing
  , \str -> if B.take 4 str == hAbc
            then Just $! runParser (abcToSwf <$> pAbcFile <* pEnd) str
            else Nothing
  ] where
      hPkZip = B.pack [0x50,0x4B]
      hUSwf  = B.pack [0x46,0x57,0x53]
      hCSwf  = B.pack [0x43,0x57,0x53]
      hAbc   = B.pack [0x10,0x00,0x2E,0x00]

      decompress str =
        let archive  = toArchive str
            swfFiles = filter (\file -> takeExtension file == ".swf") (filesInArchive archive)
            entries  = map (\file -> findEntryByPath file archive) swfFiles
            swfs     = [ runParser pSwfFile (fromEntry e) | Just e <- entries ]
        in foldr mergeSwf emptySwf swfs
