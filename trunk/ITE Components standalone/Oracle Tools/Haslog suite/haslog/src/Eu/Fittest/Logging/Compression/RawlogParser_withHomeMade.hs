{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

  This module provides the parser to parse FITTEST raw logs.

-}

{-# LANGUAGE Rank2Types, FlexibleContexts, BangPatterns #-}

module Eu.Fittest.Logging.Compression.RawlogParser(
    RawLogEntry(..),
    isParagraph,
    isSection,
    strParseRawLog,
    printStatistics,
    UTCTimeStamp1970(..)
  )

where

import Eu.Fittest.Logging.Compression.CustomParsers
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
strParseRawLog s = case parseLog s of
    (Just entries,_) -> entries
    (Nothing,t)      -> error ("** Fail to parse a rawlog:\n\n   " ++ take 200 t ++ " ...")

-- To print some basic statistics about a raw log. 
--
printStatistics :: String -> IO ()
printStatistics file = do {
    handle <- openFile file ReadMode ;
    hSetEncoding handle utf8 ;
    content <- hGetContents handle ;
    rawlog  <- return (strParseRawLog content) ;
    putStrLn ("#log-entries  = " ++ show (countTopSections rawlog)) ;
    putStrLn ("#sections     = " ++ show (countSections rawlog)) ;
    putStrLn ("#pars         = " ++ show (countPars rawlog)) ;
    ns <- return (countSentences rawlog) ;
    tns <- return (countTotLengthSent rawlog) ;
    putStrLn ("#sentences    = " ++ show (countSentences rawlog)) ;
    putStrLn ("avrg|sentence| = " ++ show ((fromInteger tns / fromInteger (toInteger ns))::Double)) ;
   }
   where 
   countTopSections log = sum [cntTopS e | e <- log]
   countSections log    = sum [cntS e | e <- log]
   countPars log        = sum [cntP e | e <- log]
   countSentences log   = sum [cntSent e | e <- log]
   countTotLengthSent log = sum [totLengthSent e | e <- log]
   cntS (Section _ _ subs) = 1 + sum [ cntS s | s <- subs ]
   cntS (Par _) = 0
   cntP (Section _ _ subs) = sum [ cntP s | s <- subs ]
   cntP (Par _) = 1

   cntTopS (Section _ _ _) = 1
   cntTopS _ = 0
   
   cntSent (Section _ _ subs) = sum [ cntSent s | s <- subs ]
   cntSent (Par sentences) = length sentences
   
   totLengthSent :: RawLogEntry UTCTimeStamp1970 String [String] -> Integer
   totLengthSent !(Section _ _ !subs) = sum [ totLengthSent s | s <- subs ]
   totLengthSent !(Par !sentences) = toInteger (length (concat sentences))
   
   
parseLog :: Parser [RawLogEntry UTCTimeStamp1970 String [String]]
parseLog = 
    skipSpaces 
    *> pSequence ((parseSection <<|> parseParagraph) <* skipSpaces)

parseSection :: Parser (RawLogEntry UTCTimeStamp1970 String [String])
parseSection = 
    (\_ _  (time,tag) _ content -> Section time tag content)
    <$> pToken sectionStart 
    <*> skipSpaces 
    <*> (  (\t _ tag -> (Just t, tag)) <$> parseTimeStamp <*> skipSpaces <*> parseSectionTag
            <<|>
           (\_ tag -> (Nothing,tag)) <$> skipSpaces <*> parseSectionTag )
    <*> skipSpaces 
    <*> pSectionPart `manyTill` (pToken endTok) 

pSectionPart :: Parser (RawLogEntry UTCTimeStamp1970 String [String])
pSectionPart = 
      (parseParagraph <<|> parseSection) 
      <* skipSpaces 

parseSectionTag :: Parser String
parseSectionTag = 
      pChar dquote 
      *>
      pAnyUntilChar dquote

parseTimeStamp :: Parser UTCTimeStamp1970
parseTimeStamp = 
       (\offset _ time -> (fromInteger offset,time))
       <$> pInteger 
       <*> pChar ':'
       <*> pInteger

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
    <*> pAnyUntilToken sentEnd
    <*> skipSpaces

p `manyTill` pEnd = (\_ -> []) <$> pEnd
                    <<|> 
                    (:) <$> p <*> (p `manyTill` pEnd)
     
    
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





