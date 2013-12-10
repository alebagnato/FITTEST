module SwfFileCheck(checkSwfFile) where

import Data.Bits
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8(fromString)
import Data.Word
import System.IO
import ParseUtil
import ByteCode
import System.FilePath


checkSwfFile :: FilePath -> IO ()
checkSwfFile path = do
  s <- B.readFile path
  let a = runParser' pSwfFile s
  seq a (putStrLn ("Checked: " ++ path))


--
-- SWF parsing
--

pSwfFile :: Parser ()
pSwfFile = do
  compressed <- pSwfHead
  version    <- pU8
  length     <- pW32
  let checkFinished p = p <* pEnd
      pTail | compressed = pDecompress' length . checkFinished
            | otherwise  = checkFinished
  pTail $ do
    size  <- padded pRect
    rate  <- pU16
    count <- pU16
    pTags

pSwfHead :: Parser Bool
pSwfHead =
  pTable [ 0x46 .> return False
         , 0x43 .> return True ]
  <* pByte 0x57
  <* pByte 0x53

pRect :: Bit Rect
pRect = do
  size <- fromIntegral <$> takeBits 5
  x1 <- takeBits size
  y1 <- takeBits size
  x2 <- takeBits size
  y2 <- takeBits size
  return (Rect_Rect size x1 x2 y1 y2)

pTags :: Parser ()
pTags = do
  t <- pTag
  case t of
    Tag_End -> return ()
    _       -> pTags

pTag :: Parser Tag
pTag = do
    key <- pU16
    let code = shiftR key 6
        len' = key .&. 0x3F
    len <- if len' == 0x3F
           then pW32
           else return (fromIntegral len')
    let kind = toTagKind code
    case kind of
      TagKind_DoABC -> emptyTag <$ pAbcFlags <* pNullString <* pAbcFile
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
                         return emptyTag
      _             -> emptyTag <$ getBytes (fromIntegral len)
  where emptyTag = Tag_Opaque TagKind_End 0 B.empty

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

pAbcFile :: Parser ()
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
    return ()

pPoolInfo :: Parser ()
pPoolInfo = ()
  <$ pCount1U30 pS32
  <* pCount1U30 pU32
  <* pCount1U30 pD64
  <* pCount1U30 pString
  <* pCount1U30 pNamespaceInfo
  <* pCount1U30 pSetInfo
  <* pCount1U30 pMultinameInfo

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
pNamespaceIndex = pU30

pSetInfo :: Parser ()
pSetInfo = () <$ pCountU30 pU30

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
pNameIndex = pU30

pSetIndex :: Parser Word32
pSetIndex = pU30

pMethodInfo :: Parser ()
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
  return ()

pLength :: Parser Int
pLength = fromIntegral <$> pU30

pMultinameIndex :: Parser Word32
pMultinameIndex = pU30

pMethodFlags :: Parser MethodFlags
pMethodFlags = pFlags [ (0x01, MethodFlag_NeedArgs)
                      , (0x02, MethodFlag_NeedAct)
                      , (0x04, MethodFlag_NeedRest)
                      , (0x08, MethodFlag_HasOptionals)
                      , (0x40, MethodFlag_SetDXNS)
                      , (0x80, MethodFlag_HasParamNames)
                      ]

pFlags :: [(Word8,a)] -> Parser [a]
pFlags flags = matchFlags flags <$> pU8

-- | takes those flags that have corresponding bits in the word
matchFlags :: [(Word8,a)] -> Word8 -> [a]
matchFlags flags b = map snd $ filter (\(x,_) -> x .&. b > 0) flags

pOptional :: Parser ()
pOptional = ()
  <$ pU32
  <* pValueKind

pValueKind :: Parser ()
pValueKind = pTable
  [ 0x03 .> return ()
  , 0x04 .> return ()
  , 0x06 .> return ()
  , 0x01 .> return ()
  , 0x0B .> return ()
  , 0x0A .> return ()
  , 0x0C .> return ()
  , 0x00 .> return ()
  , 0x08 .> return ()
  , 0x16 .> return ()
  , 0x17 .> return ()
  , 0x18 .> return ()
  , 0x19 .> return ()
  , 0x1A .> return ()
  , 0x05 .> return ()
  ]

pMetaInfo :: Parser ()
pMetaInfo = () <$ pNameIndex <* pCountU30 pMetaItem

pMetaItem :: Parser ()
pMetaItem = () <$ pNameIndex <* pNameIndex

pInstanceInfo :: Parser ()
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
    return ()

pMethodIndex :: Parser Word32
pMethodIndex = pU30

pTrait :: Parser ()
pTrait = do
    name <- pMultinameIndex
    info <- pU8
    let tp = info .&. 0x0F
    body <- case tp of
              0 -> pTraitSlot
              1 -> pTraitMethod
              2 -> pTraitMethod
              3 -> pTraitMethod
              4 -> pTraitClass
              5 -> pTraitFun
              6 -> pTraitSlot
              _ -> fail ("unknown trait type: " ++ show info)
    let flags = shiftR info 4
        attrs = matchFlags [ (0x1, TraitAttr_Final)
                           , (0x2, TraitAttr_Override)
                           , (0x4, TraitAttr_Metadata)
                           ] flags
    meta <- if TraitAttr_Metadata `elem` attrs
            then pCountU30 pMetadataIndex
            else return []
    return ()

pTraitSlot :: Parser ()
pTraitSlot = do
    slotId   <- pSlotId
    typeName <- pMultinameIndex
    vindex   <- pU30
    vkind    <- if vindex > 0
                then pValueKind
                else return ()
    return ()

pTraitMethod :: Parser ()
pTraitMethod = () <$ pDispId <* pMethodIndex

pTraitClass :: Parser ()
pTraitClass = () <$ pSlotId <* pClassIndex

pTraitFun :: Parser ()
pTraitFun = () <$ pSlotId <* pMethodIndex

pDispId = pU30
pSlotId = pU30


pClassIndex = pU30
pMetadataIndex = pU30

pClassInfo :: Parser ()
pClassInfo = () <$ pMethodIndex <* pCountU30 pTrait

pScriptInfo :: Parser ()
pScriptInfo = () <$ pMethodIndex <* pCountU30 pTrait

pBodyInfo :: Parser ()
pBodyInfo = pBodyInfo'

pBodyInfo' :: Parser ()
pBodyInfo' = ()
  <$ pMethodIndex
  <* (pU30)
  <* (pU30)
  <* (pU30)
  <* (pU30)
  <* pInstrs
  <* pCountU30 pException
  <* pCountU30 pTrait

pInstrs :: Parser ()
pInstrs = do
  n <- pU30
  p <- pPos
  pInstrArray p (p + fromIntegral n)

-- | Parses the next instruction, only if its
pInstrArray :: Int -> Int -> Parser ()
pInstrArray beginPos endPos = rec where
  rec = do hd <- pInstr
           p  <- pPos
           if p < endPos
            then rec
            else return ()

pInstr :: Parser ()
pInstr = pTable table

table :: [(Word8, Parser ())]
table =
  [ 0xA0 .> return ()
  , 0xC5 .> return ()
  , 0x9B .> return ()
  , 0x53 .> () <$ pIndex
  , 0x86 .> () <$ pMultinameIndex
  , 0x87 .> return ()
  , 0xA8 .> return ()
  , 0x97 .> return ()
  , 0xA9 .> return ()
  , 0xAA .> return ()
  , 0x01 .> return ()
  , 0xF2 .> () <$ pLine
  , 0x41 .> () <$ pArgCount
  , 0x4D .> ()  <$ pNameIndex   <* pArgCount
  , 0x43 .> ()     <$ pIndex       <* pArgCount
  , 0x46 .> ()       <$ pNameIndex   <* pArgCount
  , 0x4C .> ()    <$ pNameIndex   <* pArgCount
  , 0x4F .> ()   <$ pNameIndex   <* pArgCount
  , 0x44 .> ()     <$ pMethodIndex <* pArgCount
  , 0x45 .> ()      <$ pNameIndex   <* pArgCount
  , 0x4B .> return ()
  , 0x4E .> ()  <$ pNameIndex   <* pArgCount
  , 0x78 .> return ()
  , 0x80 .> ()         <$ pNameIndex
  , 0x81 .> return ()
  , 0x82 .> return ()
  , 0x83 .> return ()
  , 0x84 .> return ()
  , 0x85 .> return ()
  , 0x88 .> return ()
  , 0x89 .> return ()
  , 0x9A .> return ()
  , 0x42 .> ()      <$ pArgCount
  , 0x4A .> ()  <$ pNameIndex <* pArgCount
  , 0x49 .> () <$ pArgCount
  , 0x76 .> return ()
  , 0x73 .> return ()
  , 0x75 .> return ()
  , 0x77 .> return ()
  , 0x74 .> return ()
  , 0x70 .> return ()
  , 0xEF .> ()          <$ pDebugType <* pNameIndex <* pRegIndex <* pU30
  , 0xF1 .> ()      <$ pNameIndex
  , 0xF0 .> ()      <$ pLine
  , 0x94 .> ()       <$ pRegIndex
  , 0xC3 .> ()     <$ pRegIndex
  , 0x93 .> return ()
  , 0xC1 .> return ()
  , 0x6A .> () <$ pNameIndex
  , 0x6B .> return ()
  , 0xA3 .> return ()
  , 0x2A .> return ()
  , 0x06 .> ()           <$ pNameIndex
  , 0x07 .> return ()
  , 0xAB .> return ()
  , 0x72 .> return ()
  , 0x71 .> return ()
  , 0x5F .> () <$ pNameIndex
  , 0x5B .> () <$ pNameIndex
  , 0x5C .> () <$ pNameIndex
  , 0x5E .> ()   <$ pNameIndex
  , 0x5D .> () <$ pNameIndex
  , 0x59 .> () <$ pNameIndex
  , 0x64 .> return ()
  , 0x6E .> ()  <$ pSlotIndex
  , 0x60 .> ()         <$ pNameIndex
  , 0x62 .> ()       <$ pRegIndex
  , 0xD0 .> return ()
  , 0xD1 .> return ()
  , 0xD2 .> return ()
  , 0xD3 .> return ()
  , 0x67 .> ()  <$ pNameIndex
  , 0x66 .> ()    <$ pNameIndex
  , 0x65 .> () <$ pU8
  , 0x6C .> ()        <$ pSlotIndex
  , 0x04 .> ()       <$ pNameIndex
  , 0xB0 .> return ()
  , 0xAF .> return ()
  , 0x1F .> return ()
  , 0x32 .> ()       <$ pRegIndex <* pRegIndex
  , 0x13 .> ()           <$ pOffset
  , 0x12 .> ()        <$ pOffset
  , 0x18 .> ()           <$ pOffset
  , 0x17 .> ()           <$ pOffset
  , 0x16 .> ()           <$ pOffset
  , 0x15 .> ()           <$ pOffset
  , 0x0F .> ()          <$ pOffset
  , 0x0E .> ()          <$ pOffset
  , 0x0D .> ()          <$ pOffset
  , 0x0C .> ()          <$ pOffset
  , 0x14 .> ()           <$ pOffset
  , 0x19 .> ()     <$ pOffset
  , 0x1A .> ()     <$ pOffset
  , 0x11 .> ()         <$ pOffset
  , 0xB4 .> return ()
  , 0x92 .> ()       <$ pRegIndex
  , 0xC2 .> ()     <$ pRegIndex
  , 0x91 .> return ()
  , 0xC0 .> return ()
  , 0x68 .> ()   <$ pNameIndex
  , 0xB1 .> return ()
  , 0xB2 .> ()         <$ pNameIndex
  , 0xB3 .> return ()
  , 0x10 .> ()           <$ pOffset
  , 0x08 .> ()           <$ pRegIndex
  , 0x09 .> return ()
  , 0xAE .> return ()
  , 0xAD .> return ()
  , 0x38 .> return ()
  , 0x39 .> return ()
  , 0x35 .> return ()
  , 0x36 .> return ()
  , 0x37 .> return ()
  , 0x1B .> ()   <$ pOffset <* (pU30 >>= pCount pOffset . (+1) . fromIntegral)
  , 0xA5 .> return ()
  , 0xA4 .> return ()
  , 0xA2 .> return ()
  , 0xC7 .> return ()
  , 0x90 .> return ()
  , 0xC4 .> return ()
  , 0x57 .> return ()
  , 0x56 .> () <$ pArgCount
  , 0x5A .> () <$ pExceptionIndex
  , 0x58 .> () <$ pClassIndex
  , 0x40 .> () <$ pMethodIndex
  , 0x55 .> () <$ pArgCount
  , 0x1E .> return ()
  , 0x23 .> return ()
  , 0x02 .> return ()
  , 0x96 .> return ()
  , 0x29 .> return ()
  , 0x1D .> return ()
  , 0x24 .> ()       <$ pU8
  , 0x2F .> ()     <$ pDoubleIndex
  , 0x27 .> return ()
  , 0x2D .> ()        <$ pU30
  , 0x31 .> ()  <$ pNamespaceIndex
  , 0x28 .> return ()
  , 0x20 .> return ()
  , 0x30 .> return ()
  , 0x25 .> ()      <$ pU30
  , 0x2C .> ()     <$ pStringIndex
  , 0x26 .> return ()
  , 0x2E .> ()       <$ pIntIndex
  , 0x21 .> return ()
  , 0x1C .> return ()
  , 0x48 .> return ()
  , 0x47 .> return ()
  , 0xA6 .> return ()
  , 0x63 .> ()       <$ pRegIndex
  , 0xD4 .> return ()
  , 0xD5 .> return ()
  , 0xD6 .> return ()
  , 0xD7 .> return ()
  , 0x6F .> ()  <$ pSlotIndex
  , 0x61 .> ()    <$ pNameIndex
  , 0x69 .> return ()
  , 0x6D .> ()        <$ pSlotIndex
  , 0x05 .> ()       <$ pNameIndex
  , 0x50 .> return ()
  , 0x51 .> return ()
  , 0x52 .> return ()
  , 0x3D .> return ()
  , 0x3E .> return ()
  , 0x3A .> return ()
  , 0x3B .> return ()
  , 0x3C .> return ()
  , 0xAC .> return ()
  , 0xA1 .> return ()
  , 0xC6 .> return ()
  , 0x2B .> return ()
  , 0x03 .> return ()
  , 0xF3 .> return ()
  , 0x95 .> return ()
  , 0xA7 .> return ()
  ]

infix 2 .>
(.>) :: Word8 -> Parser a -> (Word8, Parser a)
k .> v = (k, v)

pArgCount = pU30
pIndex = pU30
pSlotIndex = pU30
pRegIndex = pU30
pOffset = pS24
pLine = pU30
pExceptionIndex = pU30
pIntIndex = pU30
pDoubleIndex = pU30
pStringIndex = pU30

pDebugType :: Parser Word8
pDebugType = pByte 0x01

pException :: Parser ()
pException = ()
  <$ pU30
  <* pU30
  <* pU30
  <* pNameIndex
  <* pNameIndex
