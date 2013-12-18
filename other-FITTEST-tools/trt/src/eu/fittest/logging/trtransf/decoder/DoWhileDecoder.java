package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import java.util.*; 
import org.eclipse.jdt.core.dom.*;


public class DoWhileDecoder {

	private static int freshId  = 0 ;
	
	protected static int getFreshId() {
		return freshId++ ;
	}
	
	protected static List<Statement> generate(
				CodeTransformer rootGenerator,
				CompilationUnit orgCu,
				String className,
				DoStatement node) {
			
	  List<Statement> ss = new ArrayList<Statement>();
	  boolean isLogRelevant = LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_());
			
	  if (!isLogRelevant) return ss ;
			
	  AST ast = node.getAST() ;
			
	  Expression oldGuard = node.getExpression() ;
	  Statement oldBody = node.getBody() ;;

	  // create var. firstIteration_k
	  String firstIterVarName = "firstIteration__" + getFreshId() ;
	  VariableDeclarationFragment varFirstIter = ast.newVariableDeclarationFragment() ;
	  varFirstIter.setName(ast.newSimpleName(firstIterVarName)) ;
	  varFirstIter.setInitializer(ast.newBooleanLiteral(true)) ;
	  VariableDeclarationStatement varFirstIter_ = ast.newVariableDeclarationStatement(varFirstIter) ;	  
	  varFirstIter_.setType(ast.newPrimitiveType(PrimitiveType.BOOLEAN)) ;
	    
	  ss.add(varFirstIter_) ;
	  
	  // construct a new while-loop
	  WhileStatement newWhile = ast.newWhileStatement() ;
	  Block newBody = ast.newBlock() ;
	  // true as guard:
	  newWhile.setExpression(ast.newBooleanLiteral(true)) ;
			
	  // construct an If-stmt(!firstIteration) { if (pop) break }
	  IfStatement ifStmt1 = ast.newIfStatement() ;
	  PrefixExpression guard1 = ast.newPrefixExpression() ;
	  guard1.setOperator(PrefixExpression.Operator.NOT);
	  guard1.setOperand(ast.newSimpleName(firstIterVarName)) ;
	  ifStmt1.setExpression(guard1) ;
	  	  
	  int guardLineNr = orgCu.getLineNumber(oldGuard.getStartPosition()) ;
	  int startBodyLineNr = orgCu.getLineNumber(oldBody.getStartPosition()) ;
	  int endLoopLineNr = orgCu.getLineNumber(node.getStartPosition() + node.getLength()) ;
	  IfStatement ifStmt2 = WhileDecoder.mkEncodedLoopGuard(ast,className,guardLineNr,startBodyLineNr,endLoopLineNr) ;

	  ifStmt1.setThenStatement(ifStmt2) ;
	  
	  newBody.statements().add(ifStmt1) ;
	  
	  // firstIteration := false
	  Assignment asg = ast.newAssignment() ;
	  asg.setLeftHandSide(ast.newSimpleName(firstIterVarName));
	  asg.setRightHandSide(ast.newBooleanLiteral(false)) ;
		  
	  newBody.statements().add(ast.newExpressionStatement(asg)) ;
	  
	  newBody.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldBody)) ;		
	  newBody.statements().addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, oldGuard)) ;
	  newWhile.setBody(newBody) ;
	  
	  
	  ss.add(newWhile) ;
			
	  return ss ;	
	}
	
		
}
