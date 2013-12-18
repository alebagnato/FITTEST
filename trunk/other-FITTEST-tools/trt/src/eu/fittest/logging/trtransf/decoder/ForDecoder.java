package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import java.util.*; 
import org.eclipse.jdt.core.dom.*;

public class ForDecoder {
	
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			ForStatement node) {
		
		List<Statement> ss = new ArrayList<Statement>();
		boolean isLogRelevant = LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_());
				
		if (!isLogRelevant) return ss ;
		
		AST ast = node.getAST() ;
			
		List<ASTNode> oldInits = node.initializers() ;
		Expression oldGuard = node.getExpression() ;
		if (oldGuard==null) oldGuard = ast.newBooleanLiteral(true) ;
		List<ASTNode> oldUpdaters = node.updaters() ;
		Statement oldBody = node.getBody() ;;
		
		// add the decoder of the init-part:
		ss.addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldInits)) ;

		// create var. firstIteration_k
		String firstIterVarName = "firstIteration__" + DoWhileDecoder.getFreshId() ;
		VariableDeclarationFragment varFirstIter = ast.newVariableDeclarationFragment() ;
		varFirstIter.setName(ast.newSimpleName(firstIterVarName)) ;
		varFirstIter.setInitializer(ast.newBooleanLiteral(true)) ;
		VariableDeclarationStatement varFirstIter_ = ast.newVariableDeclarationStatement(varFirstIter) ;	  
		varFirstIter_.setType(ast.newPrimitiveType(PrimitiveType.BOOLEAN)) ;    
		ss.add(varFirstIter_) ;
		  
		// construct a new while-loop
		WhileStatement newWhile = ast.newWhileStatement() ;
		Block newBody = ast.newBlock() ;
		newWhile.setBody(newBody) ;
		// true as guard:
		newWhile.setExpression(ast.newBooleanLiteral(true)) ;
		
		// add : if(!firstIteration) { decoder of updaters }
		IfStatement ifStmt1 = ast.newIfStatement() ;
		PrefixExpression guard1 = ast.newPrefixExpression() ;
		guard1.setOperator(PrefixExpression.Operator.NOT);
		guard1.setOperand(ast.newSimpleName(firstIterVarName)) ;
		ifStmt1.setExpression(guard1) ;
		Block newUpdaters = ast.newBlock() ;
		newUpdaters.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldUpdaters)) ;
		ifStmt1.setThenStatement(newUpdaters) ;
		
		newBody.statements().add(ifStmt1) ;
		
		// add the decoder of the guard:
		newBody.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldGuard)) ;
			 		
		// add: if(pop) break 
		int guardLineNr = orgCu.getLineNumber(node.getStartPosition()) ;
		int startBodyLineNr = orgCu.getLineNumber(oldBody.getStartPosition()) ;
		int endLoopLineNr = orgCu.getLineNumber(node.getStartPosition() + node.getLength()) ;
		IfStatement ifStmt2 = WhileDecoder.mkEncodedLoopGuard(ast,className,guardLineNr,startBodyLineNr,endLoopLineNr) ;
		newBody.statements().add(ifStmt2) ;
		
		// add the decoder of the loop's body
		newBody.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldBody)) ;		
	      
	    // firstIteration := false
		Assignment asg = ast.newAssignment() ;
		asg.setLeftHandSide(ast.newSimpleName(firstIterVarName));
		asg.setRightHandSide(ast.newBooleanLiteral(false)) ;  
		newBody.statements().add(ast.newExpressionStatement(asg)) ;
		
		// finally, add the new while to the decoder:
		ss.add(newWhile) ;
		
		return ss ;
	}
	


}
