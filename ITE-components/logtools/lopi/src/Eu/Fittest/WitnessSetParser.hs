{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Eu.Fittest.WitnessSetParser (getWitnesses) where

import Eu.Fittest.Data

import Data.Char

import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils 

pWitnesses :: Parser Witnesses
pWitnesses = pMany (pWitness <* pLF)

pWitness :: Parser Witness
pWitness = (\l r [p,n] t -> WitnessC t (l :~: r) (p,n)) <$> 
               pBrackets pPat
           <* pSymbol "~"
           <*> pBrackets pPat
           <*> tupleParser pIntegerRaw
           <*> (pMany $ pSatisfy (\x -> isAscii x && x /= '\n') (Insertion "insert" undefined  5))

pPat :: Parser Pat
pPat = pListSep (pSymbol ";") pSEvent

pSEvent :: Parser SEvent
pSEvent = (\en earg -> SEvent en (SEArgs earg)) <$> pId <*> (pParens pId)

getWitnesses :: String -> Witnesses
getWitnesses = runParser "error" pWitnesses

pId :: Parser String
pId = pMany (pLetter <|> pDigit <|> pSym '_')








