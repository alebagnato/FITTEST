package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;

import java.util.*; 

import org.eclipse.jdt.core.dom.*;

public class IfThenDecoder {

	/**
	 * To construct the decoder fragment that corresponds to the given
	 * if-then-else statement.
	 */
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			IfStatement node) {
		
		
		AST ast = node.getAST() ;
		
		List<Statement> ss = new ArrayList<Statement>();
		List<Statement> guardDecoder = StmtExprDecoder.generate(rootGenerator,orgCu,className,node.getExpression()) ;
		ss.addAll(guardDecoder) ;
		
		Statement oldThenPart = node.getThenStatement() ;
		Statement oldElsePart = node.getElseStatement() ;
		Block newThenBlock = ast.newBlock();
		Block newElseBlock = ast.newBlock();
		
		int guardLineNr = orgCu.getLineNumber(node.getStartPosition()) ;
		//System.out.println("### then: " + oldThenPart.getStartPosition()) ;
		int thenLineNr = orgCu.getLineNumber(oldThenPart.getStartPosition()) ;
		
		boolean thenIsLogRelevant = LogRelevance.isLogRelevantNode(oldThenPart, rootGenerator.getLRsignatures_());
		
		if (oldElsePart == null) {
	
			if (!thenIsLogRelevant) return ss ;
	
			// then part is log relevant;
			
			// create pop call
			int endIfLineNr = orgCu.getLineNumber(node.getStartPosition() + node.getLength()) ;
			MethodInvocation pop = mkPOP(ast,className,guardLineNr,thenLineNr,endIfLineNr) ;
	
			// create if statement
			IfStatement ifStatement = ast.newIfStatement();
			ifStatement.setExpression(pop);
	
			// then part is block of statements
			if (oldThenPart instanceof Block) {
				List<Statement> thenStatements = ((Block) oldThenPart).statements();			
				for (Statement s : thenStatements)
					newThenBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,s));
			}
			// then part is single statement 
			else
				newThenBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,oldThenPart)) ;
		
			ifStatement.setThenStatement(newThenBlock);
			ss.add(ifStatement);		
			
		}
		// IF-ELSE statement
		else
		{
			boolean elseIsLogRelevant = LogRelevance.isLogRelevantNode(oldElsePart, rootGenerator.getLRsignatures_());
			
			if (!thenIsLogRelevant && !elseIsLogRelevant) return ss ;
	
			// 'then' or 'else' part is log relevant;
			
			// create pop call
			int elseLineNr = orgCu.getLineNumber(oldElsePart.getStartPosition()) ;
			
			MethodInvocation pop = mkPOP(ast,className,guardLineNr,thenLineNr,elseLineNr) ;
			
			// create if statement
			IfStatement ifElseStatement = node.getAST().newIfStatement();
			ifElseStatement.setExpression(pop);
	
			// then part is block of statements
			if (oldThenPart instanceof Block) {
				List<Statement> thenStatements = ((Block) oldThenPart).statements();			
				for (Statement s : thenStatements)
					newThenBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,s));
			}
			// then part is single statement 
			else
				newThenBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,oldThenPart)) ;
		
			ifElseStatement.setThenStatement(newThenBlock);
			
			// else part is block of statements
			if (oldElsePart instanceof Block) {
				List<Statement> elseStatements = ((Block) oldElsePart).statements();			
					for (Statement s : elseStatements)
						newElseBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,s));
			}
			// then part is single statement 
			else
				newElseBlock.statements().addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,oldElsePart)) ;
			
			ifElseStatement.setElseStatement(newElseBlock);
			
			ss.add(ifElseStatement);
		}
	
		return ss;
		
	}
	
	
	/**
	 * Constructing a POP(...) expression.
	 */
	protected static MethodInvocation mkPOP(AST ast, 
			String className,
			int guardLineNr,
			int trueLineNr,
			int falseLineNr )
	{
	
		MethodInvocation pop = ast.newMethodInvocation();
		pop.setExpression(ast.newSimpleName("DLog"));
		pop.setName(ast.newSimpleName("pop"));
		
		StringLiteral guardLineNrInfo = ast.newStringLiteral() ;
		StringLiteral trueLineNrInfo = ast.newStringLiteral() ;
		StringLiteral falseLineNrInfo = ast.newStringLiteral() ;
		String className_ = (className == null || className =="") ? "?" : ("" + className) ;
		String guardLineNr_ = "" + className_ + ":" ;
		String trueLineNr_  = "" + className_ + ":" ;
		String falseLineNr_ = "" + className_ + ":" ;
		guardLineNr_ +=  (guardLineNr >= 0 ) ? guardLineNr : "?" ;
		trueLineNr_  +=  (trueLineNr >= 0 )  ? trueLineNr  : "?" ;
		falseLineNr_ +=  (falseLineNr >= 0 ) ? falseLineNr : "?" ;
				
		guardLineNrInfo.setLiteralValue(guardLineNr_) ;
		trueLineNrInfo.setLiteralValue(trueLineNr_) ;
		falseLineNrInfo.setLiteralValue(falseLineNr_) ;
	
		pop.arguments().add(guardLineNrInfo);
		pop.arguments().add(trueLineNrInfo);
		pop.arguments().add(falseLineNrInfo);
		
		return pop;
	}

}
