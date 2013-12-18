package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;

import java.util.*; 

import org.eclipse.jdt.core.dom.*;

public class StmtExprDecoder {

	/**
	 * Construct the decoder of a given statement or expression.
	 * 
	 * @param rootGenerator A code-generator; used to hold various context-infos.
	 * @param orgCu         The compilation unit to which the statement/expr belongs to.
	 * @param className     The name of the class to which the statement/expr belongs to.
	 * @param oldStmt       The target statement/expr.
	 */
	public static List<Statement> generate(
			final CodeTransformer rootGenerator,
			final CompilationUnit orgCu,
			final String className,
			ASTNode oldStmt
			) 
	{
		final List<Statement> ss = new ArrayList<Statement>();
	
		oldStmt.accept(new ASTVisitor() {
			
			// MethodInvocation
			public boolean visit(MethodInvocation node) {
				ss.addAll(MethInvocationDecoder.generate(rootGenerator, orgCu, className, node));
				return false;
			}
	
			// SuperMethodInvocation
			public boolean visit(SuperMethodInvocation node) {
				ss.addAll(SuperMethInvocationDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// new C(..)
			public boolean visit(ClassInstanceCreation node) {
				ss.addAll(ClassInstanceCreationDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// constructor call of the form this(...) 
			public boolean visit(ConstructorInvocation node) {
				ss.addAll(ConstrInvocationDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false ;
			}
	
			// InfixExpression
			public boolean visit(InfixExpression node) {
				ss.addAll(InfixExprDecoder.generate(rootGenerator, orgCu, className, node));
				return false;
			}
			
			// PrefixExpression
			public boolean visit(PrefixExpression node) {
				if (LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_())) 	
					 return  true ;
				else return false ;
			}
			
			// PostfixExpression
			public boolean visit(PostfixExpression node) {
				if (LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_()))
					// recurse
					 return true ;
				else return false ;
			}
			
			// ConditionalExpression
			public boolean visit(ConditionalExpression node) {
				ss.addAll(CondExprDecoder.generate(rootGenerator, orgCu, className, node));
				return false;
			}
						
			// IfStatement
			public boolean visit(IfStatement node) {
				ss.addAll(IfThenDecoder.generate(rootGenerator, orgCu, className, node));
				return false;
			}
			
			// SwitchStatement
			public boolean visit(SwitchStatement node) {
				ss.addAll(SwitchDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// WhileStatement
			public boolean visit(WhileStatement node) {
				ss.addAll(WhileDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// DoStatement
			public boolean visit(DoStatement node) {
				ss.addAll(DoWhileDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// ForStatement
			public boolean visit(ForStatement node) {
				ss.addAll(ForDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// Ehanced-For Statement
			public boolean visit(EnhancedForStatement node) {
				ss.addAll(EnhancedForDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// TryStatement
			public boolean visit(TryStatement node) {
				ss.addAll(TryDecoder.generate(rootGenerator, orgCu, className, node)) ;
				return false;
			}
			
			// BreakStatement
			public boolean visit(BreakStatement node) {
				AST ast = node.getAST() ;
				ss.add(insertTick(ast)) ;
				ss.add(ast.newBreakStatement()) ;
				return false;
			}
			
			// ContinueStatement
			public boolean visit(ContinueStatement node) {
				AST ast = node.getAST() ;
				ss.add(insertTick(ast)) ;
				ss.add(ast.newContinueStatement()) ;
				return false;
			}
			
			// ReturnStatement
			public boolean visit(ReturnStatement node) {
				AST ast = node.getAST() ;
				ss.add(insertTick(ast)) ;
				ss.add(ast.newReturnStatement()) ;
				return false;
			}
		});
		
		return ss;
	}

	
	protected static Statement insertTick(AST ast) {
		MethodInvocation tick = ast.newMethodInvocation();
		tick.setExpression(ast.newSimpleName("DLog"));
		tick.setName(ast.newSimpleName("tick"));
		return ast.newExpressionStatement(tick) ;
	}
	
	
	protected static List<Statement> generate(
			final CodeTransformer rootGenerator,
			final CompilationUnit orgCu,
			final String className,
			List<ASTNode> nodes
			) 
	{
		List<Statement> ss = new ArrayList<Statement>() ;
		for (ASTNode N : nodes)
			ss.addAll(generate(rootGenerator,orgCu,className,N)) ;
		return ss ;
	}
}




