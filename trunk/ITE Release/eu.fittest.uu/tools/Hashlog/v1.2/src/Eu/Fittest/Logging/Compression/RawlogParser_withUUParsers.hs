{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

  This module provides a parser for FITTEST raw-log files. It also
  defines a data type that abstractly represents such a file.

-}

{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Eu.Fittest.Logging.Compression.RawlogParser(
    RawLogEntry(..),
    isParagraph,
    isSection,
    strParseRawLog,
    printStatistics,
    UTCTimeStamp1970(..)
  )

where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
-- import Text.ParserCombinators.ReadP
import Data.Char
import Data.Time
import System.IO

-- | A representation of entries in a raw-log file. We will make it more
--   generic than needed so that we can reuse it later.
data RawLogEntry timestamp attrib paragraph
        -- | Section
        =  Section (Maybe timestamp) attrib [RawLogEntry timestamp attrib paragraph]
        -- | Paragraph
        |  Par paragraph

        deriving (Eq,Show)

isParagraph (Par p) = True
isParagraph  _      = False
isSection s = not (isParagraph s)

-- Hashlog literals
--
dquote = '\"'
sectionStart = "%<S"
endTok   = "%>"
parStart     = "%<P"
sentStart    = "%<{"
sentEnd      = "}%>"

-- | Representation of UTC timestamp as two integer (o,t)
-- where o is the location offset in minutes, and t is
-- time in terms of elapses millisecond after midnight
-- of 1st jan 1970.
--
type UTCTimeStamp1970 = (Int,Integer)

-- | To parse a string containing raw log entries.
--
strParseRawLog :: String -> [RawLogEntry UTCTimeStamp1970 String [String]]
strParseRawLog input = runParser "Error when parsing a raw-log." 
                                 parseLog 
                                 input


-- To test how fast the parser is...
--
printStatistics :: String -> IO ()
printStatistics file = do {
    handle <- openFile file ReadMode ;
    hSetEncoding handle utf8 ;
    content <- hGetContents handle ;
    rawlog  <- return (strParseRawLog content) ;
    putStrLn ("Num of sections = " ++ show (countSections rawlog)) ;
    putStrLn ("Num of pars     = " ++ show (countPars rawlog)) ;
   }
   where 
   countSections log = sum [cntS e | e <- log]
   countPars log     = sum [cntP e | e <- log]
   cntS (Section _ _ subs) = 1 + sum [ cntS s | s <- subs ]
   cntS (Par _) = 0
   cntP (Section _ _ subs) = sum [ cntP s | s <- subs ]
   cntP (Par _) = 1

parseLog :: Parser [RawLogEntry UTCTimeStamp1970 String [String]]
parseLog = (\_ sections _ -> sections)
    <$> skipSpaces 
    <*> pList ((parseSection <<|> parseParagraph) <* skipSpaces)
    <*> skipSpaces 

parseSection :: Parser (RawLogEntry (Int,Integer) String [String])
parseSection = 
    (\_ _ time _ tag _ content -> Section time tag content)
    <$> pToken sectionStart 
    <*> skipSpaces 
    <*> pMaybe parseTimeStamp 
    <*> skipSpaces 
    <*> parseSectionTag 
    <*> skipSpaces 
    <*> pSectionPart `manyTill` (pToken endTok) 

pSectionPart :: Parser (RawLogEntry (Int,Integer) String [String])
pSectionPart = 
      (parseParagraph <<|> parseSection) 
      <* skipSpaces 

parseSectionTag :: Parser String
parseSectionTag = 
      pSym dquote 
      *>
      pAnyChar `manyTill` pSym dquote

parseTimeStamp :: Parser UTCTimeStamp1970
parseTimeStamp = 
       (\offset _ time -> (fromInteger offset,time))
       <$> pIntegerRaw 
       <*> pSym ':'
       <*> pIntegerRaw 

{-
pInteger :: ReadP Integer
pInteger = do {
      sign <- option 1 ((do { char '-' ; return (-1) })
                         <++
                        (do { char '+' ; return 1 })) ;
      s <- munch1 isDigit ;
      if (sign < 0) then return (- (read s))
      else return (read s)
    }
-}

parseParagraph :: Parser (RawLogEntry UTCTimeStamp1970 String [String])
parseParagraph = 
     (\_ _ sentences -> Par sentences)
     <$> pToken parStart 
     <*> skipSpaces 
     <*> parseSentence `manyTill` (pToken endTok)

parseSentence :: Parser String
parseSentence = 
    (\_ _ s _ -> dropTrailingSpace s)
    <$> pToken sentStart 
    <*> skipSpaces 
    <*> pAnyChar `manyTill` (pToken sentEnd)
    <*> skipSpaces


-- not using UUlib's stardard pSpaces because in the future it may
-- also handle comments, which we dont want here.
skipSpaces :: Parser String
skipSpaces = pList $ pAnySym " \r\n\t" <?> "Whitespace"

manyTill :: Parser a -> Parser b -> Parser [a]
p `manyTill` pEnd  =  pEnd *> pReturn []
                      <<|>
                      (\x s-> x:s) <$> p <*> (p `manyTill` pEnd)

pAnyChar :: Parser Char
pAnyChar = pSatisfy (\_-> True) 
                    (Insertion "should not insert this!" '?' 5)

dropTrailingSpace = reverse . dropWhile isSpace . reverse

--
-- test examples:
--

exPar1 = "%<P %>"
exPar2 = "%<P %<{ }%>%>"
exPar3 = "%<P %<{ 3:int }%>%<{ undefined:void }%> \n %>"

exSec1 = "%<S \"a tag\" %>"
exSec2 = "%<S 120:1312203749927 \"a tag\" %>"
exSec3 = "%<S -120:-1312203749927 \"a tag\" %>"
exSec4 = "%<S -120:1312203749927 \"a tag\" %>"
exSec5 = "%<S -120:1312203749927 \"B:1.ET:recGcd:GCDLogged\" %>"
exSec6 = "%<S -120:1312203749927 \"FE:recGcd:GCDLogged\"\n"
         ++ "%<P %<{ null:Null }%> %>\n"
         ++ "%<S \"args\"\n"
         ++ "%<P %<{ 1:int }%> %>\n"
         ++ "%<P %<{ 3:int }%> %>\n"
         ++ "%>\n"
         ++ "%>\n"

exLog1 = do {
      log <- readFile "./SampleRawLog1.log" ;
      return (strParseRawLog log)
   }





