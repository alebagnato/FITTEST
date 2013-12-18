package eu.fittest.logging.trtransf.decoder;


import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;

import java.util.*; 

import org.eclipse.jdt.core.dom.*;

public class CondExprDecoder {

	
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			ConditionalExpression node) {
		
		AST ast = node.getAST() ;
		
		List<Statement> ss = new ArrayList<Statement>();
		List<Statement> guardDecoder = StmtExprDecoder.generate(rootGenerator,orgCu,className,node.getExpression()) ;
		ss.addAll(guardDecoder) ;
		
		Expression oldThenPart = node.getThenExpression() ;
		Expression oldElsePart = node.getElseExpression() ;

		
		int guardLineNr = orgCu.getLineNumber(node.getStartPosition()) ;
		int thenLineNr = orgCu.getLineNumber(oldThenPart.getStartPosition()) ;
		int elseLineNr = orgCu.getLineNumber(oldElsePart.getStartPosition()) ;
		
		boolean thenIsLogRelevant = LogRelevance.isLogRelevantNode(oldThenPart, rootGenerator.getLRsignatures_());
		boolean elseIsLogRelevant = LogRelevance.isLogRelevantNode(oldElsePart, rootGenerator.getLRsignatures_());
		
		if (!thenIsLogRelevant && !elseIsLogRelevant) return ss ;
		
		// else, either then-part or else-part if log-relevant
		
		// create pop call
		MethodInvocation pop = IfThenDecoder.mkPOP(ast,className,guardLineNr,thenLineNr,elseLineNr) ;
			
		// create if statement
		IfStatement ifStatement = ast.newIfStatement();
		ifStatement.setExpression(pop);
		Block newThenBlock = ast.newBlock();
		Block newElseBlock = ast.newBlock();
		newThenBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,oldThenPart));
		newElseBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,oldElsePart));
		ifStatement.setThenStatement(newThenBlock) ;
		ifStatement.setElseStatement(newElseBlock) ;
		ss.add(ifStatement) ;

		return ss ;
	}
}
