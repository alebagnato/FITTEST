package eu.fittest.logging.trtransf.decoder;

import java.util.*;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;

import org.eclipse.jdt.core.dom.*;


public class InfixExprDecoder {
	
	/**
	 * To construct the decoder fragment that corresponds to the given
	 * infix expression.
	 */
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			InfixExpression node) {
		
		AST ast = node.getAST() ;
		List<Statement> ss = new ArrayList<Statement>();
		boolean isLogRelevant = LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_());
		
		if (!isLogRelevant) return ss ;

		// the node is log relevant

		Expression e1 = node.getLeftOperand() ;
		Expression e2 = node.getRightOperand() ;
		// getting line numbers info:
		int exprLineNr = orgCu.getLineNumber(node.getStartPosition()) ;
		int e2LineNr = orgCu.getLineNumber(e2.getStartPosition()) ;
		int endExprLineNr = orgCu.getLineNumber(node.getStartPosition() + node.getLength()) ;

		// always generate for e1 (though this may result in empty-code)
		ss.addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,e1)) ;
		
		boolean isAND = node.getOperator() == InfixExpression.Operator.CONDITIONAL_AND ;
		boolean isOR  = node.getOperator() == InfixExpression.Operator.CONDITIONAL_OR ;
		
		// conditional - &&
		if (isAND || isOR) {
			
			boolean e2IsLogRelevant = LogRelevance.isLogRelevantNode(e2,rootGenerator.getLRsignatures_());
				
			if (e2IsLogRelevant) {
				
				// creating pop
				MethodInvocation pop = null ;
				if (isAND) pop = IfThenDecoder.mkPOP(ast,className,exprLineNr,e2LineNr,endExprLineNr) ;
				if (isOR)  pop = IfThenDecoder.mkPOP(ast,className,exprLineNr,endExprLineNr,e2LineNr) ;
				
				// create if statement
				IfStatement ifStatement = ast.newIfStatement();
				if (isAND) {
					ifStatement.setExpression(pop) ;
				}
				else { // isOR
					PrefixExpression neg = ast.newPrefixExpression();
					neg.setOperator(PrefixExpression.Operator.NOT);
					neg.setOperand(pop);
					ifStatement.setExpression(neg);			
				}
				// create Then block
				Block thenBlock = ast.newBlock();
				thenBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,e2)) ;
				ifStatement.setThenStatement(thenBlock);
				
				ss.add(ifStatement) ;
			}
			return ss ;
		}
		
		// the other kind of infix operators are arithmethics and relationals
		ss.addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,e2)) ;
		return ss ;
	}

}
