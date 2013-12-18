package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import java.util.*; 
import org.eclipse.jdt.core.dom.*;

public class EnhancedForDecoder {
	
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			EnhancedForStatement node) {
		
		List<Statement> ss = new ArrayList<Statement>();
		boolean isLogRelevant = LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_());
				
		if (!isLogRelevant) return ss ;
		
		AST ast = node.getAST() ;

		Expression oldGuard = node.getExpression() ;
		Statement oldBody = node.getBody() ;;

		// construct a new while-loop
		int startLoopLineNr = orgCu.getLineNumber(node.getStartPosition()) ;
		int startBodyLineNr = orgCu.getLineNumber(oldBody.getStartPosition()) ;
		int endLoopLineNr = orgCu.getLineNumber(node.getStartPosition() + node.getLength()) ;
	
		WhileStatement newWhile = WhileDecoder.mkPlainWhileDecoder(ast,orgCu,className,
				StmtExprDecoder.generate(rootGenerator, orgCu, className, oldGuard),
				StmtExprDecoder.generate(rootGenerator, orgCu, className, oldBody),
				startLoopLineNr,
				startBodyLineNr,
				endLoopLineNr
				) ;			
		ss.add(newWhile) ;
			
		return ss ;
	}

}
