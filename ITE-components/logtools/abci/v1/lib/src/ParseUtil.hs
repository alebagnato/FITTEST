-- | Framework code for the parser
--
-- Rewritten to use Data.Binary.
-- See revision 9 for the UULIB version.
{-# LANGUAGE BangPatterns #-}
module ParseUtil
  ( module Control.Monad
  , module Data.Bits
  , module Data.Word
  , module Data.Binary.Get
  , module Control.Applicative
  , pNullString, pString
  , pCount1U30, pCountU30, pCount
  , runParser, runParser'
  , pDecompress, pDecompress'
  , pPos, pEnd
  , pD64, pW32, pS32, pU32, pU30, pS24, pU16, pU8, pByte
  , pTable
  , pInfo, (<?>), pInfo'
  , Parser, parseIt, parseAny
  , Bit, takeBits, takeBit, padded
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Control.DeepSeq
import Data.Array.IArray
import Data.Bits
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Codec.Compression.Zlib as Z
import Data.Word
import System.IO
import Debug.Trace


-- | Parser type.
type Parser a = Get a

parseIt :: Parser a -> FilePath -> IO a
parseIt p = parseBase $ runParser p

parseBase :: (L.ByteString -> a) -> FilePath -> IO a
parseBase f = fmap (f . L.fromChunks . return) . B.readFile

parseAny :: [L.ByteString -> Maybe a] -> FilePath -> IO a
parseAny alts = parseBase findAlts where
  findAlts path = foldr (\f r -> maybe r id $ f path)
                        (error ("no parser applicable to input")) alts

{-# INLINE parseIt #-}
{-# INLINE parseBase #-}
{-# INLINE parseAny #-}


-- | Expects a given byte to be present in the input.
pByte :: Word8 -> Parser Word8
pByte b = do
  b' <- getWord8
  if b == b'
   then return b'
   else do pos <- bytesRead
           fail ("expecting byte " ++ show b ++ " at " ++ show pos ++ ", but found: " ++ show b')

-- | Parses an 8-bits value
pU8 :: Parser Word8
pU8 = getWord8

-- | Parses a 16-bits value in little endian
pU16 :: Parser Word16
pU16 = getWord16le

-- | Parses a 24-bits value in little endian
pS24 :: Parser Word32
pS24 = do
  w1 <- fmap fromIntegral pU8
  w2 <- fmap fromIntegral pU8
  w3 <- fmap fromIntegral pU8
  let w4 :: Word32
      w4 = if w3 `testBit` 7  -- most significant bit (sign bit) set
           then 0xFF000000    -- sign extend ones
           else 0x00000000    -- sign extend zeros
  return (w4 .|. shiftL w3 16 .|. shiftL w2 8 .|. w1)

pU30 :: Parser Word32
pU30 = pVarLength False

pU32 :: Parser Word32
pU32 = pVarLength False

pS32 :: Parser Word32
pS32 = pVarLength True

{-# INLINE pU8 #-}
{-# INLINE pU30 #-}


-- | Parses a variable length 32 bits value.
--   The MSB of each byte indicates if the next byte is also part of the value (max 5).
--   The representation is in little endian.
pVarLength :: Bool -> Parser Word32
pVarLength isSigned = accept' 1 where
  accept' n = do
    b1 <- pU8
    if n == 5 || not (b1 `testBit` 7)
     then -- this is the last byte read. Here we need to consider sign extension.
          let b | isSigned && n < 5 && b1 `testBit` 6  = 0xFFFFFF80  -- last bit is stored sign extended
                | otherwise                            = 0x00000000
          in return (b .|. fromIntegral b1)
     else do v2 <- accept' (n+1)
             let v1 = clearBit (fromIntegral b1) 7
             return (shiftL v2 7 .|. v1)

{-# INLINE pVarLength #-}

-- | 32 bits fixed size value (little endian).
pW32 :: Parser Word32
pW32 = getWord32le

-- | IEEE754 floating point value.
pD64 :: Parser Double
pD64 = getFloat64le

-- | Returns the current position in the input.
pPos :: Parser Int
pPos = fmap fromIntegral $ bytesRead

-- | Decompress a gzipped stream.
pDecompress :: Word32 -> Parser a -> Parser a
pDecompress length p = do
  s <- getRemainingLazyByteString
  let s' = Z.decompressWith Z.defaultDecompressParams { Z.decompressBufferSize = fromIntegral length } s
      x = runParser p s'
  return x

-- | Decompress a gzipped stream (stricter version).
pDecompress' :: Word32 -> Parser a -> Parser a
pDecompress' length p = do
  s <- getRemainingLazyByteString
  let s' = Z.decompressWith Z.defaultDecompressParams { Z.decompressBufferSize = fromIntegral length } s
      x = runParser' p s'
  return x

{-# INLINE pDecompress #-}

pEnd :: Parser ()
pEnd = do
  rem <- remaining
  if rem > 0
   then fail (show rem ++ " bytes remain unparsed")
   else return ()

-- | Runs the parser.
runParser :: Parser a -> ByteString -> a
runParser = runGet

-- | Runs the parser (stricter version).
runParser' :: Parser a -> ByteString -> a
runParser' p s = deepseq r a where -- Change: seq to deepseq improved time by 5 sec.
  (a,_,r) = runGetState p s 0

deepforce x = x `deepseq` x

-- Useful derived combinators

-- | Parses a given number of items.
pCount :: Parser a -> Int -> Parser [a]
pCount p !n
  | n < 0  = fail ("pCount: " ++ show n ++ " < 0.")
  | otherwise = let pCount' 0 = return []
                    pCount' n = do
                      !x <- p
                      xs <- pCount' (n-1)
                      let !res = x : xs
                      return res
                in pCount' n

-- | First parses a counter, then an equal number of items.
pCountU30 :: Parser a -> Parser [a]
pCountU30 p = fmap fromIntegral pU30 >>= pCount p

-- | First parses a counter, then parses one item less.
pCount1U30 :: Parser a -> Parser [a]
pCount1U30 p = fmap fromIntegral pU30 >>= (\n -> pCount p (0 `max` (n - 1)))

{-# INLINE pCount #-}
{-# INLINE pCountU30 #-}
{-# INLINE pCount1U30 #-}

-- | Parses a string (fixed-length)
pString :: Parser ByteString
pString = do
  n <- pU30
  getLazyByteString (fromIntegral n)
-- fmap L.pack (pCountU30 pU8) -- <?> "string"

-- | Parses a string (null-terminated)
pNullString :: Parser ByteString
pNullString = getLazyByteStringNul
-- fmap L.pack run
--  where run = pU8 >>= \c -> if c == 0 then return [] else fmap (c:) run

-- Dispatch based on the next symbol.
-- Note: relies on let/lambda-floating for efficiency.
pTable :: [(Word8, Parser a)] -> Parser a
pTable entries =
  let keys = map fst entries
      failures = [ (x, fail ("No alternative for byte: " ++ show x))
                 | x <- [0 .. 255], not (x `elem` keys) ]
      a = mkByteArray (failures ++ entries)
  in do x <- pU8
        pInfo ("parsed table-byte: " ++ show x)
        a ! x
{-# INLINE pTable #-}

mkByteArray :: [(Word8,a)] -> Array Word8 a
mkByteArray = array (0,0xFF)

pInfo :: String -> Parser ()
pInfo _ = return ()
-- pInfo = pInfo'

pInfo' ::   String -> Parser ()
pInfo' s = do
  pos <- pPos
  trace ("@" ++ show pos ++ ": " ++ s) (return ())

infix 2 <?>

(<?>) :: Parser a -> String -> Parser a
p <?> s = do
  pInfo ("started: " ++ s)
  x <- p
  pInfo ("finished: " ++ s)
  return x

{-# INLINE (<?>) #-}
{-# INLINE pInfo #-}


-- | Monad for parsing bitwise datastructures
-- on top of the binary Get interface.
-- Reads in a stream of bits, by taking bits
-- from individual bytes. The bits per byte
-- are provided in big-endian.
-- When you request a variable-sized word,
-- bits are taken from the stream. The last
-- bit read will be the least significant
-- bit in the delivered word.
type Bit a = StateT BitState Get a

data BitState = BitState { lastByte :: !Word8, lastIndex :: !Int }

padded :: Bit a -> Get a
padded m = evalStateT m (BitState 0 8)

takeBits :: Int -> Bit Word32
takeBits n
  | n < 0     = error "takeBits: n < 0"
  | otherwise = takeBits' 0 n

takeBits' :: Word32 -> Int -> Bit Word32
takeBits' acc 0 = return acc
takeBits' acc n = do
  b <- fmap toBit takeBit
  takeBits' (shiftL acc 1 .|. b) (n-1)
  where toBit False = 0
        toBit True  = 1

takeBit :: Bit Bool
takeBit = do
  i <- validIndex
  w <- gets lastByte
  increment
  return $! testBit w $ 7-i
  where
    validIndex = do
      i <- gets lastIndex
      if i > 7
       then do w <- lift pU8
               put (BitState w 0)
               return 0
       else return i
    increment = do
      modify (\s -> s { lastIndex = 1 + lastIndex s})
