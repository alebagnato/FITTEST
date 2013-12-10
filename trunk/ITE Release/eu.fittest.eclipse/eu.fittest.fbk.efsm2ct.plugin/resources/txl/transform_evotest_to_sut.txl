include "java.grm"

% rule include_java_code
% end rule

rule remove_import_1
        replace $ [repeat import_declaration]
                'import 'eu '. 'fittest '. 'efsm2ct '. 'sut '. 'TestFlexstore '; Next [repeat import_declaration]
        by
                Next
end rule

rule remove_import_2
        replace $ [repeat import_declaration]
                'import 'eu '. 'fittest '. 'fbk '. 'efsm2ct '. 'efsm2mon '. 'runtime '. 'FsmtesterException '; Next [repeat import_declaration]

        by
                Next
end rule

rule remove_import_3
        replace $ [repeat import_declaration]
                'import 'eu '. 'fittest '. 'fbk '. 'efsm2ct '. 'efsm2mon '. 'runtime '. 'NotApplicableException '; Next [repeat import_declaration]
        by
                Next
end rule

function add_logger_import
	replace * [repeat import_declaration]
			Imps [repeat import_declaration]
	by
			'import 'eu '. 'fittest '. 'fbk '. 'efsm2ct '. 'flexdrv '. 'logging '. 'LoggerManager ';
			Imps
end function

rule remove_nomore_used_imports
	replace $ [repeat import_declaration]
                Imp [repeat import_declaration]	
	by
		Imp 
			[remove_import_2]
			[remove_import_3]
end rule

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
  deconstruct Body
	'{ '}
  by
	Rest [remove_empty_tests]
end rule

rule remove_try_catch
  replace [repeat declaration_or_statement]
	S [try_statement] Rest [repeat declaration_or_statement]
  by
	Rest
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
	
function main
    replace [program]
	P [program]

    import TXLargs [repeat stringlit]
  
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
		[remove_nomore_used_imports]
		[add_logger_import]
		[rename_class  ClassName]
		[put_in_front Loaded]
		[replace_id_in_import 'TestFlexstore 'Flexstore]
		[replace_id_not_in_import 'TestFlexstore 'flexstore]		
end function
