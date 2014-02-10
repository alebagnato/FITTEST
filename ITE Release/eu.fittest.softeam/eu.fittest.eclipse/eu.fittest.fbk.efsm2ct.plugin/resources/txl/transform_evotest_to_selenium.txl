include "java.grm"

include "strlib.txl"


function change_imports
	replace [program]
		P [program]
	by
		P [remove_all_imports]
		  [add_standard_imports]
end function

rule remove_all_imports
        replace $ [repeat import_declaration]
                Imports [repeat import_declaration]
        by
                _
end rule

function add_standard_imports
	replace * [repeat import_declaration]
			Imps [repeat import_declaration]
	by
			'import 'java '. 'io '. 'File ';
			'import 'org '. 'junit '. 'Test ';
			'import 'org '. 'junit '. 'Before ';
			'import 'eu '. 'fittest '. 'flashdriver '. 'FlashApplication ';
			'import 'eu '. 'fittest '. 'flashdriver '. 'TestBase ';

			Imps
end function

function add_logger_import
	replace * [repeat import_declaration]
			Imps [repeat import_declaration]
	by
			'import 'eu '. 'fittest '. 'fbk '. 'efsm2ct '. 'flexdrv '. 'logging '. 'LoggerManager ';
			Imps
end function


function load_methods FileName [stringlit]
	replace [repeat class_body_declaration]
		_ [repeat class_body_declaration]

	construct Loaded [repeat class_body_declaration]
		_ [read FileName]

	by
		Loaded	
end function

function put_in_front Loaded [repeat class_body_declaration]
	replace * [repeat class_body_declaration]
		Meths [repeat class_body_declaration]
	by
		Loaded [. Meths]
end function

rule remove_empty_tests
  replace [repeat class_body_declaration]
	M [method_declaration] Rest [repeat class_body_declaration]
  deconstruct * [method_body] M 
	Body [method_body]
  where 
	Body [if_only_decl_body][if_empty_body]
  by
	Rest [remove_empty_tests]
end rule


rule if_empty_body
	match [method_body]
	'{ '}
end rule

rule if_only_decl_body
	match [method_body]
		'{ Stmnts [repeat declaration_or_statement]  '} 

	deconstruct Stmnts
		_ [local_variable_declaration]

end rule

rule remove_throws
  replace $ [repeat class_body_declaration]
	M [method_declaration] Rest [repeat class_body_declaration]
  
  deconstruct M
	Rm [repeat modifier] Ty [type_specifier] Md [method_declarator] 'throws 'Throwable Body [method_body]	

  construct NewM [method_declaration]
	Rm Ty Md Body

  by
	NewM Rest % [remove_empty_tests]
end rule

rule remove_try_catch
  replace [repeat declaration_or_statement]
	S [try_statement] Rest [repeat declaration_or_statement]
  by
	Rest
end rule

rule rename_constructor NewName [id]
	replace $ [constructor_declaration]
		Mods [repeat modifier] '_Constructor_ '( Args [list formal_parameter] ') Thrs [opt throws] Body [constructor_body]
	by
		Mods NewName '( Args ') Thrs Body 
end rule

rule add_class_extends BaseClass [id]
	replace $ [opt extends_clause]
		 _ [opt extends_clause]
	by
		'extends BaseClass
end rule 

rule rename_class NewName [id]
	replace $ [class_header]
		Mods [repeat modifier] 'class Name [id]
	by
		Mods 'class NewName
end rule
	
rule replace_id OldId [id] NewId [id]
	replace $ [id]
		OldId
	by
		NewId
end rule	
	
rule replace_id_not_in_import OldId [id] NewId [id]
	skipping [import_declaration]
	replace $ [id]
		OldId
	by
		NewId
end rule
		
rule replace_id_in_import OldId [id] NewId [id]
	replace $ [repeat import_declaration]
			Imp [repeat import_declaration]
	by
		Imp [replace_id OldId NewId]
end rule		

function split_in_strings_greater Ids [id]
	replace [list stringlit]
		List [list stringlit]

	construct Idx [number]
		_ [index Ids '_ ]

	deconstruct not Idx
		0
		
	construct Idxm1 [number]
		Idx [- 1]
		
	construct Idx1 [number]
		Idx [+ 1]

	construct First [id]
		Ids [: 1 Idxm1]	

	construct Str [stringlit]
		_ [quote First]

	construct TmpList [list stringlit]
		Str

	construct NewList [list stringlit]
		List [, TmpList]

	construct RestIds [id]
		Ids [: Idx1 999]

	by
		NewList [split_in_strings RestIds]

end function

function split_in_strings_equal Ids [id]
	replace [list stringlit]
		List [list stringlit]

	construct Idx [number]
		_ [index Ids '_ ]

	deconstruct Idx
		0

	construct Str [stringlit]
		_ [quote Ids]

	construct TmpList [list stringlit]
		Str

	construct NewList [list stringlit]
		List [, TmpList]

	by
		NewList
end function

function split_in_strings Ids [id]
	replace [list stringlit]
		List [list stringlit]
	by
		List [split_in_strings_greater Ids] [split_in_strings_equal Ids]
end function

rule transform_method_calls
	replace $ [statement]
		N [id] '. Meth [id] '( Args [list argument] ') ';

	where
		Meth [~= '_startup ]

	where
		Meth [~= '_shutdown ]


	construct MethSplit [list stringlit]
		_ [split_last_by_as_str '_ Meth]

	construct NewArgs [list argument]
		_ [reparse MethSplit]

	by
		'driver '. 'invoke '( NewArgs [, Args] ') ';
end rule

rule transform_variable_decls
	replace $ [local_variable_declaration]
		'int V [id] '= E [expression] ';
	by
		'String V '= 'Integer '. 'toString '( E ') ';
end rule

function wrong_usage
	match [repeat stringlit]
		args [repeat stringlit]

	deconstruct not args
		_ [stringlit] _ [stringlit] _ [repeat stringlit]

	construct _ [stringlit]
		_ [message "specify a classname and the name of an existing file"]

end function

rule add_try_catch LogPrefix [stringlit]
	replace $ [method_declaration]
		Rm [repeat modifier] Ty [type_specifier] Md [method_declarator] Th [opt throws] Body [method_body]

	deconstruct not * [modifier] Rm
		'@ 'Before

	deconstruct Md
		Name [id] '( ')

	construct TestName [stringlit]
		_ [quote Name]

	deconstruct Body
		'{ Stmnts [repeat declaration_or_statement] '}


	construct NewStmnts [repeat declaration_or_statement]
		'try '{
			Stmnts
		'}
		'finally '{
		   'String 'fname '= LogPrefix '+ "_" '+ TestName '+ "_" '+ 'System '. 'currentTimeMillis '( ') '+ ".log" ';
 		   'driver '. 'savelog '( 'fname ') ';
		'}

	construct NewBody [method_body]
		'{ NewStmnts '}

	by
		Rm Ty Md Th NewBody
end rule

function main
    replace [program]
	P [program]

    import TXLargs [repeat stringlit]

    where not
	TXLargs [wrong_usage]
  
    deconstruct TXLargs
		First [stringlit] Second [stringlit] _ [repeat stringlit]

    construct ClassName [id]
		_ [parse First]

   construct Loaded [repeat class_body_declaration]
		_ [load_methods Second]


    by
	P 
		[remove_try_catch]
		[remove_empty_tests]
		[change_imports]
		%% [add_logger_import]
		[put_in_front Loaded]
		[rename_class  ClassName]
		[add_class_extends 'TestBase ]
		[rename_constructor  ClassName]
		%% [replace_id_in_import 'TestFlexstore 'Flexstore]
		%% [replace_id_not_in_import 'TestFlexstore 'flexstore]		
		[transform_variable_decls]
		[transform_method_calls]
		[remove_throws]
		[add_try_catch First]
		%% 
end function
