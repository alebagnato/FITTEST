package eu.fittest.logging.trtransf.normalization;

import java.util.List;

import org.eclipse.jdt.core.dom.* ;

import eu.fittest.logging.trtransf.classAnalysis.LogRelevance;
import eu.fittest.logging.trtransf.tagging.TaggingCodeGenerator;
import eu.fittest.logging.trtransf.* ;

/**
 * The actual implementation of the normalization transformation; encoded as a
 * Visitor class.
 *      
 * @author Wishnu Prasetya
 *
 */
public class NormalizingVisitor extends ASTVisitor {
	
	protected Normalizer context ;
	
	/**
	 * Class to normalize.
	 */
	protected CompilationUnit cu ;
		
	/**
	 * Constructor.
	 * 
	 * @param cu   The current target class to tag.
	 */
	public NormalizingVisitor(CompilationUnit cu, Normalizer context) {
		this.cu = cu ;
		this.context = context ;
	}
	
	public boolean visit(MethodDeclaration m) {
		if (context.getLogRelevantMethods().contains(m))
			return true ;
		else 
			return false ;
	}
	
	private boolean isLogRelevant(Statement stmt) {
		return LogRelevance.isLogRelevantNode(stmt, context.getLRsignatures_()) ;
	}
	
	public boolean visit(IfStatement stmt) {
		
		if (! isLogRelevant(stmt)) return false ;
		
		Statement s1 = stmt.getThenStatement() ;
		Statement s2 = stmt.getElseStatement() ;
		AST ast = stmt.getAST() ;
		if (!(s1 instanceof Block)) {
			Block b1 = ast.newBlock() ;
			stmt.setThenStatement(b1) ;
			b1.statements().add(s1) ;
		}
		if ((s2 !=null) && !(s2 instanceof Block)) {
			Block b2 = ast.newBlock() ;
			stmt.setElseStatement(b2) ;
			b2.statements().add(s2) ;
		}
		return true ;
	}
	
	
	public boolean visit(DoStatement stmt) {
		if (! isLogRelevant(stmt)) return false ;
		wrapBody(stmt) ;
		return true ;
	}
	
	private Statement stmtGetBody(Statement stmt) {
		if (stmt instanceof DoStatement) 
			return ((DoStatement) stmt).getBody() ;
		else if (stmt instanceof WhileStatement) 
			return ((WhileStatement) stmt).getBody() ;
		else if (stmt instanceof ForStatement) 
			return ((ForStatement) stmt).getBody() ;
		else if (stmt instanceof EnhancedForStatement) 
			return ((EnhancedForStatement) stmt).getBody() ;
		else
			return ((LabeledStatement) stmt).getBody() ;
	}
	
	private void stmtSetBody(Statement stmt, Statement body) {
		if (stmt instanceof DoStatement) 
			((DoStatement) stmt).setBody(body) ;
		else if (stmt instanceof WhileStatement) 
			((WhileStatement) stmt).setBody(body) ;
		else if (stmt instanceof ForStatement) 
			((ForStatement) stmt).setBody(body) ;
		else if (stmt instanceof EnhancedForStatement) 
			((EnhancedForStatement) stmt).setBody(body) ;
		else
			((LabeledStatement) stmt).setBody(body) ;
	}
	
	private void wrapBody(Statement loop) {
		Statement oldBody = stmtGetBody(loop) ;
		AST ast = loop.getAST() ;
		if (!(oldBody instanceof Block)) {
			Block b = ast.newBlock() ;
			stmtSetBody(loop,b) ;
			b.statements().add(oldBody) ;
		}
	}
	
	public boolean visit(WhileStatement stmt) {
		if (! isLogRelevant(stmt)) return false ;
		wrapBody(stmt) ;	
		return true ;
	}
	
	public boolean visit(ForStatement stmt) {
		if (! isLogRelevant(stmt)) return false ;
		wrapBody(stmt) ;		
		return true ;
	}
	
	public boolean visit(EnhancedForStatement stmt) {
		if (! isLogRelevant(stmt)) return false ;
		wrapBody(stmt) ;		
		return true ;
	}
	
	public boolean visit(LabeledStatement stmt) {
		if (! isLogRelevant(stmt)) return false ;
		wrapBody(stmt) ;		
		return true ;
	}
	
	/*
	public boolean visit(SwitchStatement stmt) {
		if (! isLogRelevant(stmt)) return false ;
		AST ast = stmt.getAST() ;
		int N = stmt.statements().size() ;
		for (int k=0; k<N; k++) {
			Object s = stmt.statements().get(k) ;
			if (s instanceof SwitchCase) continue ;
			if (!(s instanceof Block)) {
				Block b = ast.newBlock() ;
				stmt.statements().set(k,b) ;
				b.statements().add(s) ;
			}
		}
		return true ;
	}
   */
	
	public boolean visit(SwitchStatement stmt) {
		if (! isLogRelevant(stmt)) return false ;
		AST ast = stmt.getAST() ;
		List<Statement[]> cases = CodeTransformUtils.getSwitchCases(stmt) ;
		
		stmt.statements().clear() ;
		
		for (Statement[] cs : cases) {
			stmt.statements().add(cs[0]) ;
			int N = cs.length - 1 ;
			if (N==1 && cs[1] instanceof Block) continue ;
			Block b = ast.newBlock() ;
			stmt.statements().add(b) ;
			for (int k=0; k<N; k++) b.statements().add(cs[k+1]) ;
		}
		return true ;
	}

}
