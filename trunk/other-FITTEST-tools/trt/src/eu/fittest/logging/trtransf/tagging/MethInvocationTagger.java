package eu.fittest.logging.trtransf.tagging;

import java.util.*;

import org.eclipse.jdt.core.dom.* ;

import eu.fittest.logging.trtransf.* ;
import eu.fittest.logging.trtransf.classAnalysis.*;
import eu.fittest.tloglib.* ;

/**
 * Contain some utility methods for tagging a method invocation.
 */
public class MethInvocationTagger {
	
	/**
	 * This will tag a method invocation. We also need to take dynamic binding
	 * into account. Later, the decoder needs know which variant of the method
	 * is actually called at the runtime.
	 * 
	 * Since this method is later called from a visitor-pattern, it also
	 * return a boolean to indicate whether or not recursion to the children
	 * nodes is needed. True means recursion is needed.
	 */
	protected static boolean tagMethInv(CodeTransformer context, 
			MethodInvocation minv,
			String parentClassName,
			String parentMethodName,
			int parentMethodID
			) 
	{ 	
		// DO NOT TAG added method calls such as XLog.push(..) etc; we recognize these easily because
		// all nodes added to AST have StartPosition equal to -1
		if (minv.getStartPosition() == -1) return true  ; 
		
		String msignature = SignatureUtils.getSignature(minv.resolveMethodBinding()) ;
		
		// direct call to a log-function
		if (LogRelevance.isLogCall(minv)) {
			minv.setExpression(minv.getAST().newSimpleName("TLog")) ;
			return false ;
		}
		
		// call to XLog.initializeLogger needs to be converted to DLog.innitializeLogger
		if (isLoggerInitInv(minv)) {
			transformLoggerInitInv(minv,parentClassName,parentMethodName,parentMethodID) ;
			return false ;
		}
		if (isCloseLogger(minv)) {
			transformCloseLogger(minv) ;
			return false ;
		}
			
		// check if invoked method is log relevant
		boolean isLogRelevant = SignatureUtils.contains(context.getLRsignatures_(),msignature) ;
	
		if (!isLogRelevant) return true ;
		
		// Renaming the call, as part of the solution to handle untraced calls:
		minv.getName().setIdentifier(minv.getName().getFullyQualifiedName() + "_T");
		
		// Due to dynamic binding, a method call may actually call an overriding method
		// ; this needs to be handled.
		List<String> variants = calculatePolymorphicVariants(context,minv,msignature) ;
		if (variants.size() > 1) wrapWithPolyCall(minv,variants) ;
		
		return true;
	}
	
	private static boolean isLoggerInitInv(MethodInvocation minv) {
		if (!minv.getName().toString().equals("initializeLogger")) return false ;
		if (! (minv.getExpression() instanceof Name)) return false ;
		Name o = (Name) minv.getExpression() ;
		return (o.toString().equals("XLog")) ;	
	}
	
	private static void transformLoggerInitInv(MethodInvocation minv, 
			String className, 
			String methName, 
			int methodID) {		
		AST ast = minv.getAST() ;
		minv.setExpression(ast.newSimpleName("TLog")) ;
		minv.setName(ast.newSimpleName("initializeLogger")) ;
		minv.arguments().clear() ;
		StringLiteral cName = ast.newStringLiteral() ;
		cName.setLiteralValue(className) ;
		StringLiteral mName = ast.newStringLiteral() ;
		mName.setLiteralValue(methName) ;
		minv.arguments().add(cName) ;
		minv.arguments().add(mName) ;
		minv.arguments().add(ast.newNumberLiteral(Integer.toString(methodID))) ;
		minv.arguments().add(ast.newBooleanLiteral(true)) ;
	}
	
	
	private static boolean isCloseLogger(MethodInvocation minv) {
		if (!minv.getName().toString().equals("closeLogger")) return false ;
		if (! (minv.getExpression() instanceof Name)) return false ;
		Name o = (Name) minv.getExpression() ;
		return (o.toString().equals("XLog")) ;	
	}
	
	private static void transformCloseLogger(MethodInvocation minv) {		
		AST ast = minv.getAST() ;
		minv.setExpression(ast.newSimpleName("TLog")) ;
		minv.setName(ast.newSimpleName("closeLogger")) ;
		minv.arguments().clear() ;
	}
	
	/**
	 * This is used to tag a call of the form super.m(...)
	 * 
	 * Note that such a call is statically bound. So, there is no issue of dynamic
	 * binding / variants as in the ordinary method invocation (above).
	 */
	protected static boolean tagSuperMethInv(CodeTransformer context, SuperMethodInvocation sinv) 
	{
		// DO NOT TAG added method calls such as XLog.push(..) etc --> not possible!
		
		String msignature = SignatureUtils.getSignature(sinv.resolveMethodBinding()) ;
		
		// If it is a direct call to a log-function --> not possible
			
		// check if invoked method is log relevant
		boolean isLogRelevant = SignatureUtils.contains(context.getLRsignatures_(),msignature) ;
		if (!isLogRelevant) return true ;
		
		// Renaming the call, as part of the solution to handle untraced calls:
		sinv.getName().setIdentifier(sinv.getName().getFullyQualifiedName() + "_T");
		
		// Variants due to dynamic binding  --> not possible!
		return true;
	}

	
	// create a call polycall() 
	private static MethodInvocation mkPolyCall(AST ast) {
		MethodInvocation invocation = ast.newMethodInvocation();
		invocation.setExpression(ast.newSimpleName("TLog"));
		invocation.setName(ast.newSimpleName("polycall"));
		return invocation ;
	}
	
    // Wrap a call x.m(y) with polycall, to make it :
	//   ((C) polycall(x,..)) . m(y)  
	// where C is the declared type of x.
	private static void wrapWithPolyCall(MethodInvocation minv, 
			List<String> variants)
	{
		AST nodeAst = minv.getAST();
		Expression obj = minv.getExpression();
		
		String objTy_ = obj.resolveTypeBinding().getQualifiedName() ;
		String[] tyelems = objTy_.split("\\.") ;
		int K = tyelems.length ;
		Type objTy = nodeAst.newSimpleType(nodeAst.newSimpleName(tyelems[0])) ;
		for (int i=1; i<K; i++) {
			objTy = nodeAst.newQualifiedType(objTy, nodeAst.newSimpleName(tyelems[i])) ;
		}
				
		
		// wrap o.m(x) to (Type) polyCall(o,variants).(x):
		MethodInvocation invocation = mkPolyCall(nodeAst) ;
		CastExpression castedInvocation = nodeAst.newCastExpression() ;
		castedInvocation.setExpression(invocation) ;
		castedInvocation.setType(objTy) ;
		ParenthesizedExpression castedInvocation_ = nodeAst.newParenthesizedExpression() ;
		castedInvocation_.setExpression(castedInvocation) ;
		minv.setExpression(castedInvocation_);
		//minv.setExpression(invocation);
		// as 1st argument set the expression itself
		invocation.arguments().add(obj);
		
		// as 2nd argument set the array of possible types
		
		ArrayCreation arr = nodeAst.newArrayCreation();
		arr.setType(nodeAst.newArrayType(nodeAst.newSimpleType(nodeAst.newName("Class"))));
		ArrayInitializer arrInit = nodeAst.newArrayInitializer();
		
		for (String vc : variants) {
			TypeLiteral tl = nodeAst.newTypeLiteral();
			//System.out.println("#### variant: " + vc) ;
			tl.setType(nodeAst.newSimpleType(nodeAst.newName(vc)));
			arrInit.expressions().add(tl);	
		}
		arr.setInitializer(arrInit);
		invocation.arguments().add(arr);
	}

	/**
	 * Check if a method invocation is type sensitive or not. The invocation is 
	 * represented by the signature of the invoked method m. It is type-sensitive
	 * if there is one other method m' that overrides m. This implies that at
	 * the runtime either m or m' can potentially be called.
	 */
	public static boolean isMethInvocationTypeSensitive(CodeTransformer context, String msignature) 
	{
		List<MethodDeclaration> overridings_ = context.getOverridings().get(msignature) ;
		
		return  /* overridens_.size() + */ overridings_.size() > 0 ;
	}
		
	/**
	 * Calculate all possible polymorphic variants of the given method invocation.
	 * Let x.m(..) be the invocation, where m is declared in C1 and x has a
	 * declared type C2. Note that C2 must be the same or a subclass of C1.
	 * Variants of this call are all methods m'(..) declared in the subclasses of
	 * C2. 
	 * 
	 * The variants will be represented as a list of class-names that declare them. 
	 * We also remove all abstract variants.
	 * 
	 * If no variant can be found this method will return a null. Else the invoked
	 * method itself will be added to the list of variants (more precisely, the
	 * name of C2 will be added).
	 * 
	 * @param msignature  The signature of the invoked method.
	 */
	public static List<String> calculatePolymorphicVariants(
			CodeTransformer context,
			MethodInvocation minv,
			String msignature) 
	{	
		//System.out.println("#### " + msignature) ;		
		List<String> variants = new ArrayList<String>() ;
		
		boolean typeSensitive = isMethInvocationTypeSensitive(context,msignature) ;
		
		if (typeSensitive) {
			//System.out.println("#### type sensitive: " + msignature) ;
	        Expression target = minv.getExpression() ;
		    ITypeBinding targetType = null ; // the compile-time type of the target of the method
		    if (target != null) {
			    targetType = target.resolveTypeBinding() ;
		    }
						
		    List<MethodDeclaration> overridings_ = context.getOverridings().get(msignature) ;
			
		    // sort the overriding instances, from low-class to high:
		    overridings_ = sortClassesList(overridings_) ;
			
		    // Not all overriding instances from the above list are actually variants of
		    // the given invocation, so filter the list:
		    for(MethodDeclaration v : overridings_) {
			    ITypeBinding v_class = v.resolveBinding().getDeclaringClass() ;
			    if (target != null
				    && ! SignatureUtils.isEqualOrSuperOf(targetType,v_class)) {
				    continue ;
			    }
			    if (AnalysisUtils.isAbstract(v)) continue ;
			    //System.out.println("#### " + v_class.getQualifiedName() + " NOT superOf " + targetType.getQualifiedName()) ;
			    variants.add(SignatureUtils.getFullClassName(v.resolveBinding().getDeclaringClass())) ;
		    }	
		}
		// Note that abstract methods were filterred away from the list of variants

		MethodDeclaration m = context.getSignaturesInv().get(msignature) ;
		//System.out.println("### signature: " + msignature + ", decl: " + m ) ;
		if (! AnalysisUtils.isAbstract(m)) {
		    variants.add(SignatureUtils.getFullClassName(minv.resolveMethodBinding().getDeclaringClass())) ; 
		}
		// we should have at least ONE variants:	
		assert !variants.isEmpty() ;	
		return variants ;	
	}

	/**
	 * Sort a list of method declarations according to their declaring class,
	 * in a non-decreasing order (from low to high class).
	 */
	private static List<MethodDeclaration> sortClassesList(List<MethodDeclaration> ms) {
		Object[] ms_ = ms.toArray() ;
		int N = ms_.length ;
		for (int i=0; i<N; i++) {	
			int min = i ;
			MethodDeclaration m  = (MethodDeclaration) ms_[min] ;
			for (int k=i+1; k<N; k++) {			
				MethodDeclaration m2 = (MethodDeclaration) ms_[k] ;
				if (SignatureUtils.isSuperOf(m.resolveBinding().getDeclaringClass(),
						m2.resolveBinding().getDeclaringClass())) {
					System.out.println("******  CHECK " + m.resolveBinding().getDeclaringClass().getQualifiedName() +
							" is superclass of " + m2.resolveBinding().getDeclaringClass().getQualifiedName()) ;
					min = k ;
					m = m2 ;
				}
				else
					System.out.println("******  CHECK " + m.resolveBinding().getDeclaringClass().getQualifiedName() +
							" is NOT superclass of " + m2.resolveBinding().getDeclaringClass().getQualifiedName()) ;
					
			}
			ms_[min] = ms_[i] ;
			ms_[i]   = m ;
		}
		List<MethodDeclaration> result = new LinkedList<MethodDeclaration>() ;
		for (int i=0; i<N; i++) result.add((MethodDeclaration) ms_[i]) ;
		return result ;
	}

	

}
