package eu.fittest.logging.trtransf;

import java.util.*;

import org.eclipse.jdt.core.dom.* ;

/**
 * Contain a bunch of utility methods to transform code.
 * 
 * @author Wishnu Prasetya
 *
 */
public class CodeTransformUtils {
	
	public static VariableDeclarationStatement mkNewVar(
			AST ast, 
			String name,
			Type type, 
			Expression initialization) 
	{	
		VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment() ;
		vdf.setName(ast.newSimpleName(name)) ;
		vdf.setInitializer(initialization) ;
		VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf) ;
		vds.setType(type) ;
		return vds ;
	}
	
	/**
	 * Method invocation with 0 argument.
	 */
	public static MethodInvocation mkMethodInvocation(
			AST ast,
			String name,
			Expression target
			)
	{
		Expression[] args = { } ;
		return mkMethodInvocation(ast,name,target,args) ;
	}
	
	public static MethodInvocation mkMethodInvocation(
			AST ast,
			String name,
			Expression target,
			Expression[] args 
			)
	{
		MethodInvocation mcall = ast.newMethodInvocation();
		mcall.setName(ast.newSimpleName(name));
		mcall.setExpression(target);
		for (int i=0; i<args.length; i++)
	    	mcall.arguments().add(args[i]);
		return mcall ;
	}

	/**
	 * Get case blocks of given 'switch' statement
	 * @param node Switch node
	 * @return Returns collection of case blocks
	 */
	@SuppressWarnings("unchecked")
	public static List<Statement[]> getSwitchCases(SwitchStatement node) {
		
		List<Statement> statements = node.statements();
		List<Statement[]> cases = new ArrayList<Statement[]>();
		
		int N = statements.size() ;
		int k = 0 ;
		while (k<N) {
			List<Statement> caseStatements = new ArrayList<Statement>();
			caseStatements.add(statements.get(k)) ;
			k++ ;
			while (k<N) {
				Statement s = statements.get(k) ;
				if (s instanceof SwitchCase) break ;
				caseStatements.add(s) ;
				k++ ;
			}
			cases.add(caseStatements.toArray(new Statement[0])) ;
			//caseStatements.clear() ;
		}
		return cases;
	}

	/**
	 * Tests whether defined switch statement is distinct, that is only
	 * one case can be executed at a time (that means all cases and default statement
	 * is ended with 'break', 'continue', or 'return' statement)
	 * @param node Switch node
	 */
	public static boolean isDistinctSwitch(SwitchStatement node) {
		List<Statement[]> cases = getSwitchCases(node);
		// System.out.println("** switch: " + node) ;
		// test whether each case block ends with 'break', 'return' or 'continue' statement
		for (Statement[] c : cases) {
			
			SwitchCase cw = (SwitchCase) c[0] ;
			if (cw.isDefault()) continue ;
			
			// Changing the logic:
			// Statement lastCaseStatement = c[c.length - 1];
			// the switch-cases has been normalized, so:
			Block cb = (Block) c[1] ;
			int N = cb.statements().size() ;
			// if the case-body is empty => not distinct!
			if (N==0) return false ;
			Statement lastCaseStatement = (Statement) cb.statements().get(N-1) ;
			// if does not end with statement 'return', 'break' or 'continue' => is not distinct
			if (lastCaseStatement.getClass() != BreakStatement.class
					&& lastCaseStatement.getClass() != ReturnStatement.class
					&& lastCaseStatement.getClass() != ContinueStatement.class)
				return false;
		}
		
		return true;
	}

	static public void addImport(CompilationUnit c, String pckName) {
		AST ast = c.getAST() ;
		ImportDeclaration imp = ast.newImportDeclaration() ;
		
		imp.setName(ast.newName(pckName)) ;
		c.imports().add(imp) ;
	}
	
	/**
	 * Check if a list of statements contains an unguarded return, break or throw-statement.
	 * If the list contains a block, we will check recursively.
	 */
	static public boolean hasUnguardedReturnOrBreakOrThrow(List<Statement> stmts) {
		for (Statement s : stmts) {
			if (s instanceof ReturnStatement) return true ;
			if (s instanceof BreakStatement) return true ;
			if (s instanceof ThrowStatement) return true ;
			if (s instanceof Block) {
				boolean r = hasUnguardedReturnOrBreakOrThrow(((Block) s).statements()) ;
				if (r) return true ;
			}
		}
		return false ;
	}
	
	/**
	 * Check if a list of statements contains an unguarded return or break.
	 * If the list contains a block, we will check recursively.
	 */
	static public boolean hasUnguardedReturnOrBreak(List<Statement> stmts) {
		for (Statement s : stmts) {
			if (s instanceof ReturnStatement) return true ;
			if (s instanceof BreakStatement) return true ;
			if (s instanceof Block) {
				boolean r = hasUnguardedReturnOrBreak(((Block) s).statements()) ;
				if (r) return true ;
			}
		}
		return false ;
	}

}
