{- 

Author: Wishnu Prasetya

Copyright 2013 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{-|
  This module provides a pretty printer to HTML to print Daikon oracles.
  HTML at least 4.0 is assumed.
-}
module Eu.Fittest.Oracles.Pretty.HTMLPrettyPrinter 

where

import Eu.Fittest.Oracles.Ast.OrcsDataType
import Text.Html

-- =========================================================================

color1 = "#69C5E4" 
color2 = "#3DBAE4"
color3 = "#026282"
colorWhite = "#FFFFFF"
colorRed = "#FF7373"
colorYellow = "#FFFF66"

-- =========================================================================

showTerm :: Term -> String   
showTerm t = case t of
     VariableName fields     -> foldr1 (\n r -> n ++ "." ++ r) fields
     OldVariableName fields  -> "old(" ++ foldr1 (\n r -> n ++ "." ++ r) fields ++ ")"
     MethodParam    k -> "arg" ++ show k
     OldMethodParam k -> "old(arg" ++ show k ++ ")"
     LitNull   -> "null"
     LitInt  x -> show x
     LitBool b -> show b
     LitString s -> show s
     LitDouble x -> show x
     LitIntList  s -> show s
     LitBoolList s -> show s
     LitStringList s -> show s
     LitDoubleList s -> show s
     LitIntSet  s -> curlyIt s
     LitBoolSet s -> curlyIt s
     LitStringSet s -> curlyIt s
     LitDoubleSet s -> curlyIt s
     SizeOf s    -> "|" ++ showTerm s ++ "|" 
     Binary o t1 t2  -> showTerm t1 ++ showArithBinOp o ++ showTerm t2
     
     where
     curlyIt s = "{" ++ (middle_ . show $ s) ++ "}"
          
middle_ [] = []
middle_ (opening:rest) = dropLast rest
  where
  dropLast [last] = []
  dropLast (x:s)  = x : dropLast s
  
showArithBinOp :: ArithBinOp -> String
showArithBinOp o = case o of
     Plus_    ->  " + "
     Modulo_  ->  " % "

showRelation :: Relation_ -> String
showRelation o = case o of
     GT_    ->  " &gt; "
     LT_    ->  " &lt; "
     GTE_   ->  " &ge; "
     LTE_   ->  " &le; "
     EQ_    ->  " =  "
     NotEQ_ -> " &ne; "
     EQUIV_ -> " &equiv; " 
     
showOracle :: Oracle -> String
showOracle o = case o of
     O_Relation rel t1 t2  ->  showTerm t1 ++ showRelation rel ++ showTerm t2
     O_SortedBy rel t   ->  showTerm t ++ " is sorted by " ++ showRelation rel 
     O_oneOf t1 t2      ->  showTerm t1 ++ " &isin; " ++ showTerm t2
     O_isConstant t     ->  showTerm t  ++ " has only one value"
     O_unModified t     ->  showTerm t  ++ " = old(" ++ showTerm t ++ ")"
     O_Pattern str      -> str
     
showOracle2 :: Oracle2 -> String
showOracle2 o = case o of
    Pre o_   ->  showOracle o_ ++ " &nbsp;&nbsp;&lArr; "
    Post o_  -> " &rArr;&nbsp;&nbsp; " ++ showOracle o_
    Pattern o_ -> showOracle o_
     
showEventName :: EventType -> String
showEventName ety = case ety of
   ET_AppEvent target ety_             -> target 
   ET_MethodSubcase methodName subcase -> methodName  

showEventSubcase :: EventType -> String
showEventSubcase ety = case ety of
   ET_AppEvent target ety_             -> ety_ 
   ET_MethodSubcase methodName subcase -> "visiting " ++ changeCommaToCommaSpace (show subcase)    
   where
   changeCommaToCommaSpace s = concat . map (\c-> if c == ',' then ", " else [c]) $ s
   

-- =========================================================================

empty_ :: HtmlElement
empty_ = HtmlString ""

text_ :: String -> Html
text_ s = Html [HtmlString s]  

formula_ :: String -> HtmlElement
formula_ s = HtmlTag "i" [HtmlAttr  "class" "formula"] (text_ s)
   
cell_ :: [HtmlAttr] -> [HtmlElement] -> HtmlElement
cell_ attribs s = HtmlTag "td" attribs (Html s)

row_ :: [HtmlAttr] -> [HtmlElement] -> HtmlElement
row_ attribs cells = HtmlTag "tr" attribs (Html cells)

div0_ :: [HtmlAttr] -> HtmlElement
div0_ attribs = HtmlTag "div" attribs (Html [])

div1_ :: [HtmlAttr] -> HtmlElement -> HtmlElement
div1_ attribs e = HtmlTag "div" attribs (Html [e])

table_ :: [HtmlAttr] -> [HtmlElement] -> HtmlElement
table_ attribs rows = HtmlTag "table" attribs (Html rows)

attribs z = [ HtmlAttr aname avalue | (aname,avalue) <- z ]

-- =========================================================================


-- =========================================================================
   
orcsSubGroup2Html :: String -> OracleSubGroup -> [HtmlElement]
orcsSubGroup2Html subgroupId sg@(ety, orcs) = [line,sgHeader,content]
 
   where
   
   line = div0_ [HtmlAttr "class" "subGroupSeparatorLine"] 
   
   sgHeader = div1_ (attribs [("id",subgroupId),
                              ("onclick", "colapseContent(this)") ] )
                     header
                    
        where
        header = table_ [HtmlAttr "class" header_class] 
                        [row_ [] [sg_name, button]]
        header_class = if numOfViolation>0      then "violatedSubGroupName" 
                       else if numOfUndecided>0 then "warnedSubGroupName" 
                       else "subGroupName"
                       
        sg_name =  cell_ [] [ HtmlTag "strong" [] (Html [HtmlString (showEventSubcase ety
                                                     ++ " (" ++ show numOfViolation
                                                     ++ "/"  ++ show numOfUndecided
                                                     ++ "/"  ++ show numOfOrcs
                                                     ++ ")")
                                        ]) 
                            ]
   
        buttonId = subgroupId ++ "_button"
        button = cell_  (attribs [("align","right"), ("id",buttonId)]) [HtmlString "[+] Show"]  
               
        sg_color = if numOfViolation > 0 then colorRed else color2               
        numOfViolation = countViolations sg     
        numOfUndecided = countUndecideds sg
        numOfOrcs = countOrcs sg   
   
   content = div1_ (attribs [("id",subgroupId ++ "_content"),
                             ("class","folderContent") ])
                   (div1_ [] contentTable)
   
   contentTable = table_ (attribs [("class","subGroupTable"),
                                   ("cellpadding","4"),
                                   ("cellspacing","0")])
                          oracles
                          
   oracles = [ orc2html o | o <- orcs ]
   
   orc2html (o_,i,ok) = row_ [HtmlAttr "bgcolor" orc_color] [c1,c2,c3]
      where
      c1 = cell_ [HtmlAttr "class" "subGroupCell1"] 
                 [div1_ [HtmlAttr "align" "left"] (formula_ . showOracle2 $ o_)]
      c2 = cell_ [HtmlAttr "class" "subGroupCell2"] 
                 [div1_ [HtmlAttr "align" "right"] i_]
      c3 = case ok of
             Nothing -> cell_ [HtmlAttr "class" "subGroupCell2"] []
             Just (Pass,info) ->  cell_ (attribs [("class","subGroupCell2"),("title",info)]) [div1_ [HtmlAttr "align" "center"] tick]
             Just (Violated,info) ->  cell_ (attribs [("class","subGroupCell2"),("title",info)]) [div1_ [HtmlAttr "align" "center"] cross]
             Just (Undecided,info) ->  cell_ (attribs [("class","subGroupCell2"),("title",info)]) [div1_ [HtmlAttr "align" "center"] questionmark]
              
      orc_color = case ok of
                    Just (Violated,_)      -> colorRed 
                    Just (Undecided,_) -> colorYellow
                    _                  -> colorWhite
                      
      i_  =  case i of
               Just n  -> HtmlString ("(" ++ show n ++ "x)")
               _       -> HtmlString  ""

               
cross = HtmlString "X"
tick  = HtmlString "&#10003;"
questionmark  = HtmlString "?"
               
orcsGroup2Html :: Int -> OracleGroup -> [HtmlElement]                   
orcsGroup2Html groupId (group@(groupName,subgroups)) = [line,gHeader,content]
      
   where 
   groupId_ = "G" ++ show groupId
   line = div0_ [HtmlAttr "class" "groupSeparatorLine"] 
 
   gHeader = div1_ (attribs [("id",groupId_),
                              ("onclick", "colapseContent(this)") ] )
                     header
                    
        where
        header       = table_ [HtmlAttr "class" header_class] [row_ [] [g_name, button]]
        header_class = if numOfviolation>0 then "violatedGroupName" 
                       else if numOfUndecided>0 then "warnedGroupName"
                       else "groupName"
        
        g_name =  cell_ [] [ HtmlTag "strong" [] (Html [HtmlString (groupName
                                                     ++ " (" ++ show numOfviolation
                                                     ++ "/"  ++ show numOfUndecided
                                                     ++ "/"  ++ show numOfOrcs
                                                     ++ ")")
                                                     ] ) 
                            ]                           
        buttonId = groupId_ ++ "_button"
        button   = cell_  (attribs [("align","right"), ("id",buttonId)]) [HtmlString "[+] Show"] 
                
        numOfviolation = countViolations group 
        numOfUndecided = countUndecideds group       
        numOfOrcs      = countOrcs group   
   
   content = HtmlTag "div" (attribs [("id", groupId_ ++ "_content"),
                                     ("class","folderContent") ])
                           (Html subgroups_)
    
   subgroups_ = concat [ orcsSubGroup2Html (groupId_ ++ "_" ++ show j) sg  |  (sg,j) <- zip subgroups [1..] ]
   
 
orcsSuite2Html_ :: OracleSuite -> HtmlElement                   
orcsSuite2Html_ suite =  HtmlTag "html" [] (Html [header,body])
   where
   header = HtmlTag "head" [] (Html [ pagetitle, script1, script2, css ])
   
   pagetitle = HtmlTag "title" [] (text_ "Oracles")
   
   script1 = HtmlTag "script" (attribs [("type","text/javascript"),
                                        ("src","jquery-1.10.2.js") ])
                              (Html [])
                              
   script2 = HtmlTag "script" (attribs [("type","text/javascript"),
                                        ("src","CollapsibleContent.js") ])
                              (Html []) 

   css =  HtmlTag "link" (attribs [("rel","stylesheet"),
                                   ("type","text/css"),
                                   ("href","Oracles.css") ])
                              (Html [])                            
   
   body  = HtmlTag "body" [] (Html ([title,statistics,br] ++ groups ++ [br,br,footer]))
   br    = HtmlTag "br" [] (Html [])
   title = HtmlTag "h1" [] (text_ "Oracles")
   groups = concat [ orcsGroup2Html i g | (g,i) <- zip suite [1..] ]
   footer = div1_ [HtmlAttr "id" "footer"] (HtmlString "CSS and Javascript by GPr")
   
   statistics = table_ [HtmlAttr "id" "tbl_stats"] [ntypes, nsubcases, norc, nviolations, nUndecided]
   colon     = cell_ [] [HtmlString ":"]
   ntypes    = row_ [] [ cell_ [] [HtmlString "cases"], colon, cell_ [] [HtmlString (show . length $ suite)] ]
   nsubcases = row_ [] [ cell_ [] [HtmlString "subcases"], colon, cell_ [] [HtmlString (show nSC)] ]
       where
       nSC = sum [ length subgroups | (_,subgroups) <- suite ]
   norc = row_ [] [ cell_ [] [HtmlString "oracles"], colon, cell_ [] [HtmlString (show . countOrcs $ suite)] ]
   nviolations =  row_ [] [ cell_ [] [HtmlString "violations"], 
                            colon,
                            cell_ [] [HtmlString (show . countViolations $ suite)] ] 
   nUndecided =  row_ [] [ cell_ [] [HtmlString "undecided"], 
                            colon,
                            cell_ [] [HtmlString (show . countUndecideds $ suite)] ]                              
          
 
orcsSuite2Html :: String -> OracleSuite -> IO()
orcsSuite2Html filename suite = do {
       txt <- return . prettyHtml . Html $ [orcsSuite2Html_ suite] ;
       writeFile filename txt ;  
   }

-- =========================================================================
-- some example

ex_sg1 = (ET_AppEvent "IDY_submit_button" "left-click",
            [(Pre  (O_Relation GTE_ (VariableName ["x","b","c"]) (LitInt 99)), Just (100::Int), Nothing),
             (Post (O_Relation EQ_ (VariableName ["y","a"]) (OldVariableName ["y","a"])), Just (10::Int), Nothing),
             (Post (O_unModified (VariableName ["y","a"])), Just (10::Int), Just (Violated,"Hey wrong value!"))
            ]
          )

ex_sg2 = (ET_AppEvent "IDY_submit_button" "left-click",
            [(Pre  (O_Relation GTE_ (VariableName ["x","b","c"]) (LitInt 99)), Just (100::Int), Nothing),
             (Post (O_Relation EQ_ (VariableName ["y","a"]) (OldVariableName ["y","a"])), Just (10::Int), Nothing),
             (Post (O_unModified (VariableName ["y","a"])), Just (10::Int), Just (Undecided,"not sure about this..."))
            ]
          )
          
ex_sg3 :: OracleSubGroup
ex_sg3 = (ET_AppEvent "IDY_submit_button" "right-click",
            [(Pre  (O_Relation GTE_ (VariableName ["x","b","c"]) (LitInt 99)), Just (100::Int), Nothing),
             (Post (O_Relation EQ_ (VariableName ["y","a"]) (OldVariableName ["y","a"])), Just (10::Int), Nothing),
             (Post (O_unModified (VariableName ["y","a"])), Just (10::Int), Nothing)
            ]
          )
          
ex_g1 = ("IDY_submit_button", [ex_sg1, ex_sg2]) 

ex_g2 = ("IDY_buy_button", replicate 3 ex_sg3) 


ex_suite1 = replicate 10 ex_g2 ++ [ex_g1] ++ replicate 10 ex_g2
          