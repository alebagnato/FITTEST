{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

  This module provides the parser to parse FITTEST raw logs.

-}

{-# LANGUAGE OverloadedStrings, Rank2Types, FlexibleContexts, BangPatterns #-}

module Eu.Fittest.Logging.Compression.RawlogParser(
    RawLogEntry(..),
    parseLog,
    isParagraph,
    isSection,
    strParseRawLog,
    printStatistics,
    UTCTimeStamp1970(..)
  )

where

import Prelude as P hiding (takeWhile,readFile)
-- import Eu.Fittest.Logging.Compression.CustomParsers
import Data.Char
import Data.Time
import Data.Text.IO 
import Data.Attoparsec.Text
import qualified Data.Text as T
import Text.Show
import Control.Applicative
import Control.Monad

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
dquote       = '\"'
sectionStart = "%<S"
endTok       = "%>"
parStart     = "%<P"
sentStart    = "%<{"
sentEnd      = "}%>"

-- | Representation of UTC timestamp as two integer (o,t)
-- where o is the location offset in minutes, and t is
-- time in terms of elapses millisecond after midnight
-- of 1st jan 1970.
--
type UTCTimeStamp1970 = (Int,Integer)


run' :: Parser a -> T.Text -> a
run' p s = case parseOnly p s of
             Left err -> error err
             Right r  -> r

-- | To parse a string containing raw log entries.
--
strParseRawLog :: T.Text -> [RawLogEntry UTCTimeStamp1970 String [String]]
strParseRawLog = run' parseLog 

-- To print some basic statistics about a raw log. 
--

printStatistics :: String -> IO ()
printStatistics file = do {
    content <- readFile file ;
    rawlog  <- return (strParseRawLog content) ;
    P.putStrLn ("#log-entries  = " ++ show (countTopSections rawlog)) ;
    P.putStrLn ("#sections     = " ++ show (countSections rawlog)) ;
    P.putStrLn ("#pars         = " ++ show (countPars rawlog)) ;
    ns <- return (countSentences rawlog) ;
    tns <- return (countTotLengthSent rawlog) ;
    P.putStrLn ("#sentences    = " ++ show (countSentences rawlog)) ;
    P.putStrLn ("avrg|sentence| = " ++ show ((fromInteger tns / fromInteger (toInteger ns))::Double)) ;
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


pToken = string
{-# INLINE pToken #-}
pInteger = signed decimal
{-# INLINE pInteger #-}

untilInclc :: Char -> Parser String
-- untilInclc c = let !x = show <$> manyTill (notChar c) (char c) in x
untilInclc c = ({-# SCC "show" #-}T.unpack) <$> {-# SCC "takeW" #-} 
                    (takeWhile ({-# SCC "cmp" #-}(/= c)) <* ({-# SCC "char" #-} char c))


untilSentEnd :: Parser String
-- untilP p = show <$> let !x = manyTill anyChar p in x
untilSentEnd = T.unpack <$> (let !x = takeWhile (/= '}') <* string sentEnd in x)

parseLog :: Parser [RawLogEntry UTCTimeStamp1970 String [String]]
parseLog = 
    skipSpace 
    *> many1 ((parseSection <|> parseParagraph) <* skipSpace)

parseSection :: Parser (RawLogEntry UTCTimeStamp1970 String [String])
parseSection = 
    Section
    <$ pToken sectionStart 
    <* skipSpace 
    <*> option Nothing (Just <$> parseTimeStamp)
    <*  skipSpace 
    <*> parseSectionTag
    <* skipSpace 
    <*> parseSectionPart `manyTill` (pToken endTok) 


parseSectionPart :: Parser (RawLogEntry UTCTimeStamp1970 String [String])
parseSectionPart = (parseParagraph <|> parseSection) <* skipSpace 

parseSectionTag :: Parser String
parseSectionTag = 
      char dquote 
      *>
      untilInclc dquote

parseParagraph :: Parser (RawLogEntry UTCTimeStamp1970 String [String])
parseParagraph = 
      Par
      <$ pToken parStart 
      <* skipSpace 
      <*> parseSentence `manyTill` (pToken endTok)
     
parseSentence :: Parser String
parseSentence = 
    (\s -> dropTrailingSpace s)
    <$ pToken sentStart 
    <* skipSpace 
    <*> untilSentEnd
    <* skipSpace

parseTimeStamp :: Parser UTCTimeStamp1970
parseTimeStamp = 
       (\offset time -> (fromInteger offset,time))
       <$> pInteger 
       <* char ':'
       <*> pInteger

       
       
{-
p `manyTillW` pEnd = (\_ -> []) <$> pEnd
                    <<|> 
                    (:) <$> p <*> (p `manyTillW` pEnd)
-}

pParens op p cl = op *> p <* cl

dropTrailingSpace = reverse . dropWhile isSpace . reverse

--
-- test examples:
--

exPar1 = "%<P %>" :: T.Text


exPar2 = "%<P %<{ }%>%>"  :: T.Text
exPar3 = "%<P %<{ 3:int }%>%<{ undefined:void }%> \n %>"  :: T.Text

exSec1 = "%<S \"a tag\" %>"  :: T.Text
exSec2 = "%<S 120:1312203749927 \"a tag\" %>"  :: T.Text
exSec3 = "%<S -120:-1312203749927 \"a tag\" %>"  :: T.Text
exSec4 = "%<S -120:1312203749927 \"a tag\" %>"  :: T.Text
exSec5 = "%<S -120:1312203749927 \"B:1.ET:recGcd:GCDLogged\" %>"  :: T.Text
exSec6 = "%<S -120:1312203749927 \"FE:recGcd:GCDLogged\"\n%<P %<{ null:Null }%> %>\n%<S \"args\"\n%<P %<{ 1:int }%> %>\n%<P %<{ 3:int }%> %>\n%>\n%>\n"  :: T.Text

exLog1 = do {
      log <- readFile "./SampleRawLog1.log" ;
      return (strParseRawLog log)
   }




