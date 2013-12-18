package eu.fittest.logging.trtransf.classAnalysis;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.core.dom.*;


/**
 * Provide methods related to deciding if a section of code or a method is
 * log-relevant.
 * 
 * @author Ales Sturala, Wishnu Prasetya
 *
 */
public class LogRelevance {
	
	/**
	 * Returns true if the given method invocation is Xlog.log(...).
	 */	
	public static Boolean isLogCall(MethodInvocation invocation) {
		if ((invocation.getName().getFullyQualifiedName().equals("log")) 
		     && invocation.getExpression() instanceof SimpleName
		     && ((SimpleName)invocation.getExpression()).getFullyQualifiedName().equals("XLog")) {
			return true;
		}
		return false ;
	}
	
	/**
	 * Returns true if the given method or constructor declaration directly 
	 * calls Xlog.log(...) in its body.
	 */	
	public static Boolean isMethodCallingLog(MethodDeclaration method) {
		final List<Boolean> result = new ArrayList<Boolean>();
		method.accept(new ASTVisitor() {
			public boolean visit(MethodInvocation node) {
				if (LogRelevance.isLogCall(node)) result.add(true) ;
				return true;
			}
		});
		// if result has one element, then there was at least one call to
		// log:
		return result.size()>0 ;
	}
	
	
	/**
	 * Returns all method or constructor declarations that in their body 
	 * directly calls XLog.log(...).
	 */
	public static List<MethodDeclaration> getLogCallingMethods(List<CompilationUnit> cus) {
		List<MethodDeclaration> methods = new ArrayList<MethodDeclaration>();
		for (CompilationUnit c : cus) methods.addAll(LogRelevance.getLogCallingMethods(c)) ;
	    return methods ;
	}
	
	/**
	 * Returns all method and constructor declarations that in their body 
	 * directly calls XLog.log(...).
	 */
	public static List<MethodDeclaration> getLogCallingMethods(CompilationUnit cu) {
		// here keep log calling method declarations
		final ArrayList<MethodDeclaration> methods = new ArrayList<MethodDeclaration>();
		
		// give me all classes in current compilation unit
		List<TypeDeclaration> classes = AnalysisUtils.getAllClasses(cu);
		
		// check each class
		for (TypeDeclaration cls : classes) {
			// give me all methods of current class
			List<MethodDeclaration> clsMethods = AnalysisUtils.getDeclaredMethods(cls);
			
			// check each method in current class
			for (MethodDeclaration method : clsMethods) {
				if (LogRelevance.isMethodCallingLog(method))
					methods.add(method);
			}
		}

		return methods;
	}

		
	
	/**
	 * Returns the declarations of all "log-relevant" methods and constructors.
	 * A method is log-relevant if it directly calls Xlog.log(..), or it calls
	 * another log-relevant method.
	 * 
	 * Additionally, methods overriden by a log-relevant method is also considered
	 * as log-relevant.
	 * 
	 * WP NOTE: in original version, a method that overrides a log-relevant method is
	 * also automatically considered as log-relevant. The original argument given
	 * by Ales in his thesis is not completely correct.
	 * So, this logic is now TURNED OFF.
	 * 
	 */
	public static List<MethodDeclaration> getLogRelevantMethods(List<CompilationUnit> cus) {
		// here we keep found log relevant methods
		List<MethodDeclaration> logRelevantMethods = new ArrayList<MethodDeclaration>();

		// here we keep all methods that were already tested if they are log relevant
		//List<MethodDeclaration> testedMethods = new ArrayList<MethodDeclaration>();

		// here we keep all methods that need to be tested yet
		List<MethodDeclaration> methodsToTest = LogRelevance.getLogCallingMethods(cus) ;

		while (methodsToTest.size() > 0) {
			// process first method in the list and remove it from the list of methods to test
			MethodDeclaration method = methodsToTest.get(0);
			methodsToTest.remove(method);

			// since each method in the methodsToTest is there because it 
			// calls a log relevant method, add it to log relevant methods as well
			logRelevantMethods.add(method);
			IMethodBinding mbinding = method.resolveBinding() ;

			// All methods that override a log relevant method are log relevant as well.
			// This sounds strange ... see Ales' explanation in his thesis about this.
			/*
			List<MethodDeclaration> overridingMethods = AnalysisUtils.getOverridingMethods(cus,mbinding) ;
			// test if method is not already in the list of methods to test or log relevant methods,
			// if it is then do not add again
			for (MethodDeclaration m : overridingMethods) {
				//System.out.println(method.resolveBinding().getDeclaringClass().getName() + "." + method.getName() + " overrides " + m.resolveBinding().getDeclaringClass().getName() + "." + m.getName());
				if (!methodsToTest.contains(m) && !logRelevantMethods.contains(m))
					methodsToTest.add(m);
			}
			*/
			
			// All methods that are overridden by the current a log relevant method; they are log relevant as well
			List<MethodDeclaration> overriddenMethods = AnalysisUtils.getMethodsOverridenByMethod(cus,mbinding);

			// test if method is not already in the list of methods to test or log relevant methods,
			// if it is then do not add again
			for (MethodDeclaration m : overriddenMethods) {
				// System.out.println(method.resolveBinding().getDeclaringClass().getName() + "." + method.getName() + " overrides " + m.resolveBinding().getDeclaringClass().getName() + "." + m.getName());
				if (!methodsToTest.contains(m) && !logRelevantMethods.contains(m))
					methodsToTest.add(m);
			}

			// give me all methods that call currently tested method
			List<MethodDeclaration> callingMethods = AnalysisUtils.getMethodsCallingMethod(cus,mbinding);
			callingMethods.addAll(AnalysisUtils.getMethodsCallingConstructor(cus,mbinding));

			// test if each calling method was already tested or is in list of tested methods (and thus is log relevant)
			for (MethodDeclaration m : callingMethods) {
				if (!methodsToTest.contains(m) && !logRelevantMethods.contains(m))
					methodsToTest.add(m) ;
			}
		}

		return logRelevantMethods ;
	}

	/**
	 * Determines whether an AST node is log-relevant. Note that the definition of log-relevant 
	 * node is slightly different than log-relevant method. This is simply defined a
	 * isLogRelevantNode(node,logRelevantMethods,false). Check the doc of the latter.
	 */
	public static Boolean isLogRelevantNode(ASTNode node, 
			final List<String> logRelevantMethods) {		
		return LogRelevance.isLogRelevantNode(node, logRelevantMethods, false);
	}
	
	/**
	 * Determines whether an AST node is log-relevant. The definition of log-relevant node is
	 * slightly different than log-relevant method.
	 * 
	 * A node is log-relevant if it contains a direct call to XLog.log(..), or a call to a
	 * log-relevant method. It is also considered as log-relevant if its normal (non-exceptional)
	 * control flow can affect whether or not a logging-statement outside the node will be
	 * executed or avoided.
	 * 
	 * The latter means that additionally the node is also also considered as log-relevant 
	 * if it is part of a log-relevant method/constr and it contains a "return" statement.
	 * 
	 * We will not actually check if the node is part of a log-relevant method/constr. We will
	 * rely on the upper layer code that will NOT recurse into the body of a method/constr
	 * if it is not log-relevant.
	 * 
	 * Additionally, if the node is part of a loop that calls XLog.log(..) directly or indirectly,
	 * and the node contains a break or continue, then the node can influence the logging in the
	 * loop. So, the node should also considered as log-relevant. However, in the implementation
	 * below we will not actually check if the loop calls XLog.log(..) etc. We will rely on the
	 * the upper layer code that will NOT recurse into the sub-nodes of a loop if the loop does
	 * not call XLog.log(...) etc.
	 * 
	 * Additionally, to allow re-use elsewhere (e.g. when dealing with switch. NOTE: this is actually
	 * not used anymore!), we will also
	 * add a flag checkForBreak. If this flag is set to true, then the node will be considered
	 * as log-relevant if it contains break or continue, regardless whether or not it is part
	 * of a loop.
	 * 
	 */
	public static boolean isLogRelevantNode(
			ASTNode node, 
			final List<String> logRelevantMethods, 
			boolean checkForBreak) 
	{	
		
		final FinalResult<Boolean> result = new FinalResult<Boolean>(false) ;
		final FinalResult<Boolean> includeBreakCont = new FinalResult<Boolean>(checkForBreak) ;	
		
		ASTNode currentNode = node.getParent();

		// check if node is inside a loop
		currentNode = node.getParent();
		while (currentNode != null) {
			// one of the node's parents is a method declaration
			if (currentNode instanceof WhileStatement
					|| currentNode instanceof DoStatement
					|| currentNode instanceof ForStatement
					|| currentNode instanceof EnhancedForStatement
					) {
				includeBreakCont.value = true;
				break;
			}			
			currentNode = currentNode.getParent();
		}
		
		// check if node in its subtree calls a log relevant method, calls log or instantiates log relevant constructor 
		node.accept(new ASTVisitor() {
			// check if log relevant method is called in subtree
			public boolean visit(MethodInvocation node) {	
				if (result.value) return false ;  // no-recursion
				if (SignatureUtils.containsSig(logRelevantMethods,node) || isLogCall(node)) result.value = true ;		
				return true;
			}
			
			// check if log relevant constructor is invoked in subtree
			public boolean visit(ClassInstanceCreation node) {
				if (result.value) return false ; // no-recursion
				if (SignatureUtils.containsSig(logRelevantMethods,node)) result.value = true ;
				return true;
			}
			
			// check if 'return' statement is present in subtree (if included)
			public boolean visit(ReturnStatement node) {
				if (result.value) return false ; // no-recursion
				result.value = true ;			
				return true;
			}
			
			// check if 'break' statement is present in subtree (if included)
			public boolean visit(BreakStatement node) {
				if (includeBreakCont.value) result.value = true ;
				return false ; // no-recursion ... not possible anyway
			}
			
			// check if 'continue' statement is present in subtree (if included)
			public boolean visit(ContinueStatement node) {
				if (includeBreakCont.value) result.value = true ;			
				return false ; // no-recursion ... not possible anyway
			}
		});

		return result.value ;
	}
	
	
	/**
	 * Determines whether at least one node in the given collection is log relevant.
	 */
	public static boolean isLogRelevantNodeCollection(List<ASTNode> nodes, final List<String> logRelevantMethods) {	
     	// check the results;
		// if at least one node is log relevant, the whole collection is considered as log relevant 
		for (ASTNode node : nodes) {		
			if (LogRelevance.isLogRelevantNode(node, logRelevantMethods))
				return true;
		}		
		return false;
	}

	/**
	 * Tests whether given case block is log relevant, that means
	 * that at least one of its statements is log relevant.
	 * @param caseBlock Case block
	 */
	public static boolean isSwitchCaseLogRelevant(Statement[] caseBlock, List<String> logRelevantMethods, boolean checkForBreak) {

		// test each case's statement if it is log relevant
		for (Statement s : caseBlock) {
			
			boolean caseIsLogRelevant = LogRelevance.isLogRelevantNode(s, logRelevantMethods, checkForBreak);
			
			// log relevant node found => return true
			if (caseIsLogRelevant)  return true;
		}
		
		return false;
	}
}





