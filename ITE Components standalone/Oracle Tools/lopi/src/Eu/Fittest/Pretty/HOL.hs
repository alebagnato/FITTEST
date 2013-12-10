{-# LANGUAGE ImpredicativeTypes #-}

module Eu.Fittest.Pretty.HOL 
       ( ppLog
       , ppWitnesses
       , ppAEvents
       ) where

import Eu.Fittest.Data
import Text.PrettyPrint.HughesPJ
import Data.Char

ppLog :: Log -> Doc
ppLog log = text "val"    <+> 
            text "logfile" <+>
            text "="      $$
            (ppHolTerm $ ppHolList $ map ppLogEntry log) <>
            semi
            
ppHolList :: [Doc] -> Doc
ppHolList docs = brackets $ vcat $ punctuate semi docs            

ppMLList :: [Doc] -> Doc
ppMLList docs = brackets $ vcat $ punctuate comma docs

ppHolTerm :: Doc -> Doc
ppHolTerm doc = text "Term" <+> bquotes doc 

-- wrap doc in arbitrary (user-defined) brackets, passed as the first and second args to the function metaWrap
metaWrap :: String -> String -> Doc -> Doc
metaWrap op cl doc = text op <> doc <> text cl

-- back quote brackets
bquotes :: Doc -> Doc
bquotes doc = metaWrap "`" "`" doc

-- double dash parentheses 
ddparens :: Doc -> Doc
ddparens doc = metaWrap "(--" "--)" doc

ppLogEntry :: LogEntry -> Doc
ppLogEntry (LogEntry st ev) = char '^' <> 
                              (parens $ text "state_" <+>
                                       (ddparens $ bquotes $ ppState st) <+>
                                       (parens   $ ppCEvent ev)
                              )

ppState :: State -> Doc
ppState (State st) = brackets $ hcat $ punctuate semi $ map ppVar st

ppVar :: Var -> Doc
ppVar v = text v

ppCEvent :: CEvent -> Doc
ppCEvent (CEvent en args) = text en <+>
                            (ddparens $ bquotes (
                                (brackets $ hcat $ punctuate semi $ map text args) <>  text ":int list")
                            )

-- | Pretty printing of rewrite rules in hol represenation.  
ppWitnesses :: [Witness] -> Doc
ppWitnesses ws = text "val" <+>
                 text "facts" <+>
                 text "=" <+>
                 text "map" <+>
                 text "ASSUME" $$
                 (ppMLList $ map ppWitness ws) <>
                 semi
                       
ppWitness :: Witness -> Doc
ppWitness (WitnessC _ (lhs :~: rhs)  t) = 
  ppHolTerm $ 
  (
    ( text $ 
      toUpperString $ 
      show t
    ) <+>
    ( hsep $ 
      map (doubleQuotes . text . symEName) lhs
    )
  )

toUpperString :: String -> String
toUpperString = map toUpper

-- | Pretty print event definitions as hol terms.
ppAEvents :: [AEvent] -> Doc
ppAEvents evts = vcat $ map ppAEvent evts

ppAEvent :: AEvent -> Doc
ppAEvent ae = ppHolFun ae ["p"] (ppHolTerm $ ppHolEvent ae "p")
              
ppHolFun :: String -> [String] -> Doc -> Doc
ppHolFun fn args body = text "fun" <+>
                        text fn <+> 
                        (parens $ hcat $ punctuate comma (map text args)) <+>
                        text "=" <+>
                        body <> semi
                        
ppHolTuple :: [Doc] -> Doc
ppHolTuple ts = metaWrap "<|" "|>" $ hsep $ punctuate semi ts 

ppHolAssign :: String -> Doc -> Doc
ppHolAssign lhs rhs = text lhs <+> text ":=" <+> rhs
                        
ppHolEvent :: String -> String -> Doc
ppHolEvent en eargs = ppHolTuple $ [ ppHolAssign "etype" (doubleQuotes $ text en)
                                   , ppHolAssign "eparam" (text "^" <> 
                                                           text eargs
                                                          )
                                   ]