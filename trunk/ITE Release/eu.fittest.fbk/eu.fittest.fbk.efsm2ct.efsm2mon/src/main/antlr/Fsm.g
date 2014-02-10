grammar Fsm;


@header {
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.*;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.FsmTesterException;

import java.io.Reader;
import java.io.IOException;
import org.antlr.runtime.RecognitionException;
import java.util.Map;
import java.util.HashMap;

}

@lexer::header { 
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser;
}

@members {


    public static class FsmRecognitionException extends RecognitionException {
        
        private String message;

        public FsmRecognitionException(TokenStream ts, String msg) {
            
                super(ts);
                message = msg;

        }
    }

    private Model model = new Model();
    private int ns = 0;
    private Map<String,Mutator> mutatorsMap = new HashMap<String,Mutator>();
    private Map<String,Inspector> inspectorsMap = new HashMap<String,Inspector>();

    public static Model parse(Reader is) throws FsmTesterException  {

        try {
            FsmLexer lexer = new FsmLexer(new ANTLRReaderStream(is));
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            FsmParser parser = new FsmParser(tokens);
            parser.fsm();
            return parser.model;
        } catch (Throwable ex) {
            throw new FsmTesterException("can'parse", ex);
        }
    }
 
    
    @Override
    public void reportError(RecognitionException e) {
    
    	System.out.println(e);
    
    	throw new RuntimeException("can't recover from: "+e);
    
    }

/*
    @Override
    public boolean mismatchIsMissingToken(IntStream input, BitSet follow) {

        return false;

    }
    
    */
    
    
    /*
    @Override
	protected Object recoverFromMismatchedToken(IntStream input, int ttype, BitSet follow) throws RecognitionException	{  
	    
	    System.out.println("can't recoverFromMismatchedToken");
	    
	    throw new MismatchedTokenException(ttype, input);
	}
	*/
	
	
	/*
	@Override
	public void recover(IntStream input, RecognitionException re) {
		throw new RuntimeException("can't recover from: "+re);
	} 
	*/
	
	 
}


@lexer::members {

    // a very rude way to handle tokenizer error but we didn't find any better way to do it

    public void recover(RecognitionException re) {
        System.err.println("fsmtest: ERROR: "+re);
        throw new RuntimeException(re);
    }
}

/*
@rulecatch {
    catch(RecognitionException ex) {
        System.err.println("fsmtest: CATCHED ERROR: "+ex);
        // reportError(ex);
        // roberto ...
        throw ex;
    }
}
*/

/*Parser rules */

fsm :
     FSM classPath O_CURL 
        mutators_decl
        inspectors_decl 
        states_decl
        transitions_decl
    C_CURL { model.setClassPath($classPath.text);}
    ;


classPath : ID ( DOT ID )*;

states_decl : STATES O_CURL state_stmt* C_CURL ;

transitions_decl : TRANSITIONS O_CURL transition_stmt* C_CURL ;

mutators_decl : MUTATORS O_CURL mutator_decl* C_CURL ;

mutator_decl : v=ID ASSIGN mid=ID {Mutator m = new Mutator(); m.setName($mid.text); m.setAlias($v.text); model.addMutator(m); mutatorsMap.put($v.text,m); } O_PAR formal_args_list[m] C_PAR SEMICOLON ;

inspectors_decl : INSPECTORS O_CURL inspector_decl * C_CURL  ;

inspector_decl : type a=ID ASSIGN v=ID O_PAR C_PAR SEMICOLON { 
    Inspector ins = new Inspector(); ins.setType($type.text);
    ins.setName($v.text); inspectorsMap.put($a.text,ins); 
    model.addInspector(ins); };

formal_args_list[Mutator m] : (formal_arg[m] ( COMMA formal_arg[m] )*)? ;

formal_arg[Mutator m] : type {m.addArgumentType($type.text); } ;

type : 
    INT | CHAR | STRING ;

state_stmt @init { State q = new State();} : ID (initial[q])? O_CURL predicate_list[q] C_CURL SEMICOLON {
        // q.setInitial(true);
        // q.setPredicate("n == 0");
        q.setId(ns++);
        q.setName($ID.text);
        model.addState(q);
    };

initial[State q] : O_SQR INITIAL C_SQR {
        model.setInitialState(q);
        q.setInitial(true);
    };

// predicate_list[State q] : ( p=predicate SEMICOLON { q.addAtomic($p.text); }  )+ ;
predicate_list[State q] : ( p=predicate SEMICOLON { q.setPredicate($p.r); } ) ;

transition_stmt : src=ID ARROW trg=ID O_CURL mutid=ID SEMICOLON C_CURL SEMICOLON { 

    State srcs = model.getStateByName($src.text);
    
    if (srcs == null) {

        throw new RuntimeException("unknown src state:"+$src.text);

    }

    State trgs = model.getStateByName($trg.text);
    

    if (trgs == null) {

        throw new RuntimeException("unknown target state:"+$trg.text);

    }

    Transition t = new Transition(); 

    t.setSource(srcs);
    t.setTarget(trgs);

    Mutator m = mutatorsMap.get($mutid.text);
    m.addTransition(t);
    model.addTransition(t);

};

predicate returns [String r] : 
    a = and_exp ( OR a1 = and_exp { $a.r += " || "+$a1.r; } )* { $r = $a.r; };

and_exp returns [String r] :
    p = logical_primary ( AND p1 = logical_primary { $p.r += " && "+$p1.r; }  )* { $r = $p.r; };

logical_primary returns [String r] :
    atom  { $r = $atom.r; } ;

atom returns [String r] :
    exp cmp NUM {

         $r = $exp.r;

         $r += " "+$cmp.text+" "+$NUM.text;

    } 
    | TRUE { $r = "true"; } 
    | FALSE { $r = "false"; }
    ;


exp returns [String r] :
     primary { $r=$primary.r; } 
     ; 


primary returns [String r] :
    id { $r = $id.r; }
    | id '.' ID '(' ')' { $r = $id.r+"."+$ID.text+"()"; }
    ;

id returns [String r] :
ID {
        String alias=$ID.text;

        // System.out.println("DEBUG: searching for: "+alias);

        if (inspectorsMap.containsKey(alias)) {

        $r = inspectorsMap.get(alias).getName();

        // System.out.println("DEBUG: found: "+n); 

        } else {

            throw new FsmRecognitionException(input,"SSem: unknown inspector: "+alias);
        
        }
    }
    ;


cmp :   EQ | NEQ | GT | GE | LT | LE ;


/* Lexer */

FSM : 'fsm' ;
MUTATORS : 'mutators' ;
INSPECTORS : 'inspectors' ;
STATES : 'states' ;
TRANSITIONS : 'transitions' ;
INITIAL : 'initial' ;
INT : 'int';
CHAR : 'char' ;
STRING : 'String' ;
TRUE : 'true';
FALSE : 'false';


ASSIGN : ':=' ;
EQ : '==' ;
NEQ : '!=' ; 
GT : '>' ;
GE : '>=' ;
LT : '<' ; 
LE : '<=' ;
OR : '||';
AND : '&&' ;

SEMICOLON : ';' ;
COMMA : ',' ;
O_PAR : '(' ;
C_PAR : ')' ;
O_SQR : '[' ;
C_SQR : ']' ;
O_CURL : '{' ;
C_CURL : '}' ;
ARROW : '->' ;
DOT : '.' ;
MINUS : '-';

LCOMMENT : '#' ~ ('\r' | '\n' )* ('\r' | '\n' ) {skip();};


ID : ('A'..'Z' | 'a'..'z' | '_' )('A'..'Z' |'a'..'z' | '0'..'9' | '_' )*  ;

STR   : '"' ~('"')* '"' ; 

WS      : (' '|'\t' | NL )+ {skip();} ;

fragment
NL      : '\r'? '\n' ;

// COMMA   : ',';

// REF   : ('A'..'Z')('A'..'Z' | '0'..'9')* ('+' ('1'..'2'))? ;
NUM   : MINUS ? ('0'..'9')+; // ('0'..'9' | 'a'..'f' | 'A'..'F')* ;
