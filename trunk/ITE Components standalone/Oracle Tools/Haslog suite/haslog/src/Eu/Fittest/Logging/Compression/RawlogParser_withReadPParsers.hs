{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

  This module provides a parser for FITTEST raw-log files. It also
  defines a data type that abstractly represents such a file.

-}

module Eu.Fittest.Logging.Compression.RawlogParser(
    RawLogEntry(..),
    isParagraph,
    isSection,
    strParseRawLog,
    printStatistics,
    UTCTimeStamp1970(..)
  )

where

import Text.ParserCombinators.ReadP
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
strParseRawLog input = runParser parseLog input

runParser p s = fst . last $ (readP_to_S p s)

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


parseLog :: ReadP [RawLogEntry UTCTimeStamp1970 String [String]]
parseLog = do {
      skipSpaces ;
      sections <- endBy (parseSection <++ parseParagraph) skipSpaces ;
      return sections
   }

parseSection :: ReadP (RawLogEntry (Int,Integer) String [String])
parseSection = do {
    string sectionStart ;
    skipSpaces ;
    time <- option Nothing (do { t <- parseTimeStamp; return (Just t) }) ;
    skipSpaces ;
    tag <- parseSectionTag ;
    skipSpaces ;
    content <- pSectionPart `manyTill` (string endTok) ;
    return (Section time tag content) ;
   }

pSectionPart = do {
      part <- (parseParagraph <++ parseSection) ;
      skipSpaces ;
      return part ;
    }

parseSectionTag = do {
      char dquote ;
      tag <- pAny `manyTill` char dquote ;
      return tag ;
    }

parseTimeStamp :: ReadP UTCTimeStamp1970
parseTimeStamp = do {
       offset <- pInteger ;
       char ':';
       time <- pInteger ;
       return (fromInteger offset,time) ;
    }

pInteger :: ReadP Integer
pInteger = do {
      sign <- option 1 ((do { char '-' ; return (-1) })
                         <++
                        (do { char '+' ; return 1 })) ;
      s <- munch1 isDigit ;
      if (sign < 0) then return (- (read s))
      else return (read s)
    }

parseParagraph = do {
     string parStart ;
     skipSpaces ;
     sentences <- parseSentence `manyTill` (string endTok) ;
     return (Par sentences) ;
    }

parseSentence = do {
    string sentStart ;
    skipSpaces ;
    s <- pAny `manyTill` (string sentEnd) ;
    skipSpaces ;
    return (dropTrailingSpace s) ;
    }

pAny = satisfy (\_-> True)
dropTrailingSpace = reverse . dropWhile isSpace . reverse


exPar1 = "%<P %>"
exPar2 = "%<P %<{ }%>%>"
exPar3 = "%<P %<{ 3:int }%>%<{ undefined:void }%>%> \n %>"

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





