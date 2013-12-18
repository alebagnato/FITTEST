package eu.fittest.logging.trtransf.classAnalysis;

import java.util.*;

import org.eclipse.jdt.core.dom.*;


/**
 * Utilities to get and check the names/types of AST-nodes, in particular methods,
 * to check signature-based method equalities, and overriding.
 * 
 * @author Wishnu Prasetya
 *
 */
public class SignatureUtils {

	public static String getFullMethodName(IMethodBinding mb) {
		return  getFullClassName(mb.getDeclaringClass()) 
				+ "." + mb.getName() ;
	}

	public static String getFullMethodName(MethodDeclaration m) {
		return getFullMethodName(m.resolveBinding()) ;
	}

	public static String getFullMethodName(MethodInvocation m) {
		return getFullMethodName(m.resolveMethodBinding().getMethodDeclaration()) ;
	}

	/**
	 * Get the parameters-signature of a method. This is essentially the concatenation
	 * of the binary-names of the types of the method's formal parameters.
	 */
	public static String getTypeSignature(IMethodBinding mb){
		 ITypeBinding[] argtypes = mb.getParameterTypes() ;
		 String signature = "" ;
		 for (int i=0; i<argtypes.length; i++) signature += "#" + argtypes[i].getBinaryName() ;
		 return signature ;
	}
	
	/**
	 * Get the signature of a method. It consists of its full name, followed by
	 * its parameters-signature.
	 */
	public static String getSignature(IMethodBinding mb){
		return getFullMethodName(mb) + getTypeSignature(mb) ;
	}	
	
	/**
	 * Return the simple signature of a method. This is its simple name, followed by
	 * its parameters-signature.
	 */
	public static String getSimpleSignature(IMethodBinding mb){
		return mb.getName() + getTypeSignature(mb) ;
	}
	
	/**
	 * Check whether two AST nodes representing two methods actually represent
	 * the same methods. The nodes may originate from different ASTs. So, we
	 * can't just check this based on pointer equality.
	 */
	public static Boolean eqMethod(IMethodBinding m1, IMethodBinding m2) {
		if (m1==m2) return true ;
		
		// Unfortunately this does not work, because methods from outside
		// the AST is keyed differently:
		// return m1.resolveBinding().getKey().equals(m2.resolveBinding().getKey()) ;
		
		// This is not complete, because names do not uniquely identify methods: 
		// return (getFullMethodName(m1).equals(getFullMethodName(m2))) ;
		assert (m1 != null) ;
		assert (m2 != null) ;
		return (getSignature(m1).equals(getSignature(m2))) ;
	}

	public static Boolean eqMethod(MethodInvocation m1, MethodDeclaration m2) {
		IMethodBinding m1_ = m1.resolveMethodBinding() ;
		IMethodBinding m2_ = m2.resolveBinding() ;
		//System.out.print(">> " + m1_.getKey()) ;
		//System.out.println(" xx " + m2_.getKey()) ;
		// return m2_.getKey().equals(m1_.getKey()) ;
		// System.out.print(">> " + getSignature(m1_)) ;
		// System.out.println(" xx " + getSignature(m2_)) ;
		return eqMethod(m1_,m2_) ;	
	}
	
	public static String getFullClassName(ITypeBinding c) {
		return c.getQualifiedName() ;
		// return c.getPackage().getName() + "." + c.getName() ;
	}
	
	/**
	 * Give all superclasses, including c itself.
	 */
	private static void getAllSuperClasses(ITypeBinding c, List<ITypeBinding> result) {
		result.add(c) ;
		ITypeBinding sprclass = c.getSuperclass() ;
		if (sprclass != null) getAllSuperClasses(sprclass,result) ;
	}
	
	/**
	 * Give the names of all strict superclasses of c (excluding c itself).
	 */
	public static List<String> getAllSuperClassNames(ITypeBinding c) {
		List<String> names = new ArrayList<String>() ;
		ITypeBinding d = c.getSuperclass() ;
		if (d == null) return names ;
		List<ITypeBinding> supers = new ArrayList<ITypeBinding>() ;
		getAllSuperClasses(d,supers) ;
		for (ITypeBinding e : supers) {
			names.add(getFullClassName(e)) ;
		}	
		return names ;
	}
	
	/**
	 * True if c1 is a strict superclass of c2; else false.
	 */
	public static Boolean isSuperOf(ITypeBinding c1, ITypeBinding c2) {
		String c1name = getFullClassName(c1) ;
	    for (String dname : getAllSuperClassNames(c2)) {
	    	if (c1name.equals(dname)) return true ;
	    }
	    return false ;
	}
	
	/**
	 * True if c1 is equal or a strict superclass of c2; else false.
	 */
	public static Boolean isEqualOrSuperOf(ITypeBinding c1, ITypeBinding c2) {
		String c1name = getFullClassName(c1) ;
		if (c1name.equals(getFullClassName(c2))) return true ;
	    for (String dname : getAllSuperClassNames(c2)) {
	    	if (c1name.equals(dname)) return true ;
	    }
	    return false ;
	}
	
	/**
	 * True if m1 overrides m2. This happens if they have the same simple signature,
	 * but the class of m2 is a superclass of m1.
	 */
	public static Boolean overrides(IMethodBinding m1, IMethodBinding m2) {
		return getSimpleSignature(m1).equals(getSimpleSignature(m2))
		       && isSuperOf(m2.getDeclaringClass(),m1.getDeclaringClass()) ;
	}

	/**
	 * Returns log method declared in the code.
	 * 
	 * WP:DROPPED.
	 *
	public static MethodDeclaration GetLogMethodDeclaration(String projectName) {
		final List<MethodDeclaration> logDeclarations = new ArrayList<MethodDeclaration>();
		
		List<CompilationUnit> cus = JdtProject.GetCompilationUnits(projectName);
		for (CompilationUnit cu : cus) {
			cu.accept(new ASTVisitor() {
				public boolean visit(MethodDeclaration node) {
					if (node.getName().getFullyQualifiedName().equals("write")
							&& node.resolveBinding().getDeclaringClass().getName().equals("Log"))
						logDeclarations.add(node);
					
					return true;
				}
			});
		}
		// log declaration not found
		if (logDeclarations.size() == 0) {
			System.out.println("WARNING: Log.write() method not found in project");
			return null;
		}
		
		// multiple declarations of log found
		if (logDeclarations.size() >= 2)
			System.out.println("WARNING: there are multiple log declarations found in the project; only first is used");
		
		
		return logDeclarations.get(0);
	}
	*/
	
	public static boolean contains(List<String> names, String n) {
		for (String s : names) {
			if (n.equals(s)) return true ; 
		}
		return false ;
	}

	public static boolean containsSig(List<String> names, ClassInstanceCreation constructor) {
		return contains(names, getSignature(constructor.resolveConstructorBinding())) ;
	}

	public static boolean containsSig(List<String> names, MethodInvocation m) {
		return contains(names, getSignature(m.resolveMethodBinding())) ;
	}

}
