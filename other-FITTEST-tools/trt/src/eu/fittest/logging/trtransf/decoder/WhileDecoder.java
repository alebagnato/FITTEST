package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import java.util.*; 
import org.eclipse.jdt.core.dom.*;

public class WhileDecoder {

	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			WhileStatement node) {
		
        List<Statement> ss = new ArrayList<Statement>();
		boolean isLogRelevant = LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_());
		
		if (!isLogRelevant) return ss ;
		
		AST ast = node.getAST() ;
		
		Expression oldGuard = node.getExpression() ;
		Statement oldBody = node.getBody() ;;

		
		// construct a new while-loop
		int startWhileLineNr = orgCu.getLineNumber(node.getStartPosition()) ;
		int startBodyLineNr = orgCu.getLineNumber(oldBody.getStartPosition()) ;
		int endWhileLineNr = orgCu.getLineNumber(node.getStartPosition() + node.getLength()) ;
	
		WhileStatement newWhile = mkPlainWhileDecoder(ast,orgCu,className,
				StmtExprDecoder.generate(rootGenerator, orgCu, className, oldGuard),
				StmtExprDecoder.generate(rootGenerator, orgCu, className, oldBody),
				startWhileLineNr,
				startBodyLineNr,
				endWhileLineNr
				) ;			
		ss.add(newWhile) ;
		
		return ss ;	
	}
	
	// construct while(true) { if(pop) break ; decoder-body }
	protected static WhileStatement mkPlainWhileDecoder(
			AST ast,
			CompilationUnit orgCu,
			String className,	
			List<Statement> guardDecoder,
			List<Statement> bodyDecoder,
			int startWhileLineNr,
			int startBodyLineNr,
			int endWhileLineNr
			) {

		WhileStatement newWhile = ast.newWhileStatement() ;
		// true as guard:
		newWhile.setExpression(ast.newBooleanLiteral(true)) ;
				
		Block newBody = ast.newBlock() ;
		newWhile.setBody(newBody) ;		
		
		// add guard's decoder
		newBody.statements().addAll(guardDecoder) ;
		// add if(pop()) break ;
		IfStatement ifStmt = mkEncodedLoopGuard(ast,className,startWhileLineNr,startBodyLineNr,endWhileLineNr) ;
		newBody.statements().add(ifStmt) ;
		// add body's decoder
		newBody.statements().addAll(bodyDecoder) ;
		
		return newWhile ;
	}
	
	// Construct if(!pop(..)) break:
	protected static IfStatement mkEncodedLoopGuard(AST ast,
			   String className,
			   int guardLineNr,
			   int startBodyLineNr,
			   int loopExitLineNr
			) {
		IfStatement ifStmt = ast.newIfStatement() ;
		ifStmt.setThenStatement(ast.newBreakStatement()) ;
		MethodInvocation pop = IfThenDecoder.mkPOP(ast,className,guardLineNr,startBodyLineNr,loopExitLineNr) ;
		PrefixExpression guard = ast.newPrefixExpression() ;
		guard.setOperand(pop) ;
		guard.setOperator(PrefixExpression.Operator.NOT);
		ifStmt.setExpression(guard) ;
		
		return ifStmt ;
	}
	
}
