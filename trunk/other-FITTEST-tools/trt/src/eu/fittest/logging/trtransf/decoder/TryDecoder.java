package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;

import java.util.*; 

import org.eclipse.jdt.core.dom.*;

public class TryDecoder {
	
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			TryStatement node) {
		
		List<Statement> ss = new ArrayList<Statement>();
		boolean isLogRelevant = LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_());
		
		if (!isLogRelevant) return ss ;
		
		AST ast = node.getAST() ;
		
		Block oldBody = node.getBody() ;
		Block oldFInally = node.getFinally() ;
		List<CatchClause> oldHandlers = node.catchClauses() ;
		
		// add a tick before the entire try construct
		ss.add(StmtExprDecoder.insertTick(ast)) ;
		
		TryStatement newTry = ast.newTryStatement() ;
		// constructing new-body
		Block newBody = ast.newBlock() ;
		newBody.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldBody)) ;
		if (! CodeTransformUtils.hasUnguardedReturnOrBreak(oldBody.statements()))
		      newBody.statements().add(StmtExprDecoder.insertTick(ast)) ;
		newTry.setBody(newBody) ;
		
		// constructing the new handlers
		for (CatchClause oldH : oldHandlers) {
			CatchClause h = ast.newCatchClause() ;
			SingleVariableDeclaration e = (SingleVariableDeclaration) ASTNode.copySubtree(ast, oldH.getException()) ;
			h.setException(e) ;
			Block newHandlerBody = ast.newBlock() ;
			newHandlerBody.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldH.getBody())) ;
			if (! CodeTransformUtils.hasUnguardedReturnOrBreak(oldH.getBody().statements()))
			      newHandlerBody.statements().add(StmtExprDecoder.insertTick(ast)) ;
			h.setBody(newHandlerBody) ;
			newTry.catchClauses().add(h) ;
		}
		
		// contructing the finally-part
		if (oldFInally != null) {
		Block newFinally = ast.newBlock() ;
		  newFinally.statements().add(enterFinally(ast)) ;
		  newFinally.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldFInally)) ;
		  if (! CodeTransformUtils.hasUnguardedReturnOrBreak(oldFInally.statements()))
		        newFinally.statements().add(StmtExprDecoder.insertTick(ast)) ;
		  newTry.setFinally(newFinally) ;
		}
		
		ss.add(newTry) ;
		return ss ;
	}
	
	
	private static Statement enterFinally(AST ast) {
		MethodInvocation tick = ast.newMethodInvocation();
		tick.setExpression(ast.newSimpleName("DLog"));
		tick.setName(ast.newSimpleName("enterFinally"));
		return ast.newExpressionStatement(tick) ;
	}

}
