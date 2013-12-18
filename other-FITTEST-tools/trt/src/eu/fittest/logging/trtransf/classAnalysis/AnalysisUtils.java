package eu.fittest.logging.trtransf.classAnalysis;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.jdt.core.dom.*;

import eu.fittest.logging.trtransf.classAnalysis.SignatureUtils ;

/**
 * Provide various utility methods to analyze a compilation unit.
 *
 * @author Ales Sturala, Wishnu Prasetya
 *
 */
public class AnalysisUtils {
	
	
	/**
	 * Print an analysis on a compilation unit.
	 * @param cu
	 */
	static public void analyzeClass(CompilationUnit cu) {
		List<CompilationUnit> cus = new ArrayList<CompilationUnit>() ;
		cus.add(cu) ;
		analyzeClass(cus) ;
	}
	
	/**
	 * Print an analysis on a compilation unit.
	 * @param cu
	 */
	static public void analyzeClass(List<CompilationUnit> cus) {
		
		List<TypeDeclaration> classes = new ArrayList<TypeDeclaration>() ;
		for (CompilationUnit c : cus) classes.addAll(AnalysisUtils.getAllClasses(c)) ;
				
		System.out.println("** #classes: " + classes.size());
		
		for (TypeDeclaration cls : classes) {
			System.out.print("\n** class: " + cls.getName());
			List<MethodDeclaration> methods = AnalysisUtils.getDeclaredMethods(cls);	
			System.out.println(", #methods : " + methods.size());
			
			for (MethodDeclaration method : methods) {
				System.out.print("   method: " + method.getName());
				List<Expression> invocations = AnalysisUtils.getMethodInvocations(method);
				List<Expression> innerInvocations = AnalysisUtils.getInnerMethodInvocations(method);
				
				System.out.print(", #invocations: " + invocations.size());
				System.out.println(",  #inner-invocations: " + innerInvocations.size());
			}
		}
	}
		
	/**
	 * get all classes in compilation unit
	 */
	public static List<TypeDeclaration> getAllClasses(CompilationUnit cu) {	
		// here keep parsed classes
		final ArrayList<TypeDeclaration> classes = new ArrayList<TypeDeclaration>();
		
		cu.accept(new ASTVisitor() {
			public boolean visit(TypeDeclaration node) {
				// node is not class => return
				if (!node.resolveBinding().isClass())
					return true;
				
				//System.out.println("Type declaration " + node.getName());				
				classes.add(node);
				return true;
			}
		});
		
		return classes;
	}

	
	public static List<MethodDeclaration> getAllMethods(List<CompilationUnit> cus) {	
		List<MethodDeclaration> methods = new ArrayList<MethodDeclaration>() ;
		for (CompilationUnit c : cus) 
			methods.addAll(getAllMethods(c)) ;
		return methods ;
	}
	
	/**
	 * get all methods in compilation unit
	 */
	public static List<MethodDeclaration> getAllMethods(CompilationUnit cu) {
		List<TypeDeclaration> classes = AnalysisUtils.getAllClasses(cu);
		
		// here keep found methods
		List<MethodDeclaration> methods = new ArrayList<MethodDeclaration>();

		for (TypeDeclaration cls : classes)
			methods.addAll(AnalysisUtils.getDeclaredMethods(cls));
		
		return methods;
	}
	
	/**
	 * For given class type binding returns its class declaration (for binding returns node)
	 */
	/*
	public static TypeDeclaration getClassDeclaration(ITypeBinding cls, CompilationUnit cu) {
		List<TypeDeclaration> classes = AnalysisUtils.getAllClasses(cu);
		for (TypeDeclaration c : classes) {
			if (c.resolveBinding() == cls)
				return c;
		}
		
		return null;
	}
	*/
	
	/**
	 * For given method binding returns its method declaration (for binding returns node)
	 */
	/*
	public static MethodDeclaration getMethodDeclaration(IMethodBinding binding, CompilationUnit cu) {
		List<MethodDeclaration> methods = AnalysisUtils.getAllMethods(cu);
		for (MethodDeclaration m : methods) {
			if (m.resolveBinding() == binding)
				return m;
		}
		
		return null;
	}
    */
	
	/***
	 * Determines whether a given class is an ancestor of another class
	 * @param cls class
	 * @param ancestor class to test if is ancestor of the first class
	 * @param cu Compilation Unit
	 * @return
	 */
	/*
	public static Boolean isAncestorOf(TypeDeclaration cls, ITypeBinding ancestor, CompilationUnit cu) {
		// given class is not inherited from any other class (either null or java.lang.Object)
		if (cls.resolveBinding().getSuperclass() == null
				|| cls.resolveBinding().getSuperclass() == cls.getAST().resolveWellKnownType("java.lang.Object")) 
			return false;

		// given class is directly inherited from the ancestor
		if (cls.resolveBinding().getSuperclass() == ancestor)
			return true;
		//System.out.println(((ITypeBinding)cls.resolveBinding().getSuperclass()).getName());
		
		// give me node of class's super class
		TypeDeclaration superClassNode = AnalysisUtils.getClassDeclaration(cls.resolveBinding().getSuperclass(), cu);
		
		// given class is inherited from other class; check ancestor of the super class
		return AnalysisUtils.isAncestorOf(superClassNode, ancestor, cu);
	}
	*/
	
	/***
	 * Determines whether a given class is an ancestor of another class
	 * @param cls class
	 * @param ancestor class to test if is ancestor of the first class
	 * @param cu Compilation Unit
	 * @return
	 */
	/*
	public static Boolean isAncestorOf(TypeDeclaration cls, TypeDeclaration ancestor, CompilationUnit cu) {
		return AnalysisUtils.isAncestorOf(cls, ancestor.resolveBinding(), cu);
	}
	*/
	
	/**
	 * get all descendants of a particular class
	 */
	/*
	public static List<TypeDeclaration> getDescendants(TypeDeclaration cls, CompilationUnit cu) {
		
		// here keep parsed classes
		ArrayList<TypeDeclaration> descendants = new ArrayList<TypeDeclaration>();
		
		// give me all code classes
		List<TypeDeclaration> classes = AnalysisUtils.getAllClasses(cu);
		
		// check for each class in the code if it is descendant of given class
		for (TypeDeclaration c : classes) {
			// if given class is ancestor of current class, then add current class into the list
			if (AnalysisUtils.isAncestorOf(c, cls, cu))
				descendants.add(c);
		}
		
		return descendants;
	}
	*/
	
	/**
	 * get all methods declared within a class.
	 */
	public static List<MethodDeclaration> getDeclaredMethods(final TypeDeclaration cls) {
		// here keep parsed method declarations
		final ArrayList<MethodDeclaration> methods = new ArrayList<MethodDeclaration>();
		
		cls.accept(new ASTVisitor() {
			public boolean visit(MethodDeclaration node) {
				//System.out.println("MethodDeclaration: " + node.getName());				
				methods.add(node);
				return true;
			}
			
			public boolean visit(TypeDeclaration node) {	
				if (!node.equals(cls)) {
					// if you meet another class declared inside of this class do not traverse
					// the nested classes and their methods
					return false;
				} else
					return true;
			}
		});
		
		return methods;
	}
	
	/**
	 * get all method invocations within a method declaration;
	 * returned List contains objects only of type MethodInvocation or SuperMethodInvocation.
	 */
	public static List<Expression> getMethodInvocations(MethodDeclaration  method) {
		// here keep parsed method invocations within a method
		final ArrayList<Expression> invocations = new ArrayList<Expression>();
		
		method.accept(new ASTVisitor() {
			public boolean visit(MethodInvocation node) {
				//System.out.println("MethodInvocation: " + node.getName());
				invocations.add(node);
				return true;
			}
			
			public boolean visit(SuperMethodInvocation node) {
				//System.out.println("SuperMethodInvocation: " + node.getName());				
				invocations.add(node);
				return true;
			}			
		});
		
		return invocations;
	}

	/**
	 * Get invocations of methods dispatched to the same class or its ancestors;
	 * those are methods using 'this' keyword, 'super' keyword or without a invocation target.
	 */
	private static List<Expression> getInnerMethodInvocations(MethodDeclaration method) {
		
		List<Expression> invocations = AnalysisUtils.getMethodInvocations(method);
		
		List<Expression> tobeRemoved = new ArrayList<Expression>() ;
		
		for (Expression e : invocations) {
			if (!(e instanceof MethodInvocation)) continue ;
			MethodInvocation inv = (MethodInvocation) e ;
			if (inv.getExpression() != null && !(inv.getExpression() instanceof ThisExpression))
			    tobeRemoved.add(e) ;		
		}
		
		invocations.removeAll(tobeRemoved) ;
		
		return invocations;
	}
	
	
    /***
	 * Having given method invoked on given class, return closest ancestor that
	 * implements the method or the class itself if the method is implemented
	 * by the class itself
	 * @param cu Compilation Unit
	 * @param method Called method
	 * @param clss Class on which the method is called
	 * @return Returns the closest method implementation
	 */
	/*
	public static IMethodBinding findClosestImplementingClass(CompilationUnit cu, MethodDeclaration method, TypeDeclaration clss) {
		TypeDeclaration currentClass = clss;
		while (currentClass != null) {
			// give me all methods implemented by current class
			List<MethodDeclaration> methods = AnalysisUtils.getDeclaredMethods(currentClass);
			
			// check if one of the method is the searched method;
			// the comparison is made only on names, overloading is not supported
			for (MethodDeclaration m : methods) {
				// compare method names
				if (m.getName().equals(m.getName())) {
					// method found, return the class that implements it
					return m.resolveBinding();
				}	
			}
			
			currentClass = (TypeDeclaration)cu.findDeclaringNode(currentClass.resolveBinding().getSuperclass());
		}
		
		// class not found; the method cannot be called on the class
		// because it is not implemented by the class or any of its ancestors
		throw new Error("Given method does not implement any class");		
	}
	*/

	/**
	 * Get all methods that override the given method.
	 */
	public static List<MethodDeclaration> getOverridingMethods(List<CompilationUnit> cus, IMethodBinding method) {
		ArrayList<MethodDeclaration> methods = new ArrayList<MethodDeclaration>() ;
		for (CompilationUnit c : cus)
			methods.addAll(getOverridingMethods(c,method)) ;
	    return methods ;
	}

	/**
	 * Get all methods that override the given method.
	 */
	public static List<MethodDeclaration> getOverridingMethods(CompilationUnit cu, IMethodBinding method) {
		// here store all methods that @method overrides
		List<MethodDeclaration> overridenMethods = new ArrayList<MethodDeclaration>();
		
		// give me all declared methods
		List<MethodDeclaration> methods = getAllMethods(cu);
		
		// test if each method is overridden by given @method
		for (MethodDeclaration m : methods) {
			if /*(m.resolveBinding().overrides(method.resolveBinding())) */
			   (SignatureUtils.overrides(m.resolveBinding(), method))
			   { 
				overridenMethods.add(m);
				//System.out.println(m.resolveBinding().getDeclaringClass().getName() + "." + m.getName());
			}
		}
		
		return overridenMethods;
	}

	/**
	 * Returns all method declarations that in their body directly call a given constructor.
	 */
	public static List<MethodDeclaration> getMethodsCallingConstructor(List<CompilationUnit> cus, IMethodBinding calledConstructor) {
		ArrayList<MethodDeclaration> constuctors = new ArrayList<MethodDeclaration>() ;
		for (CompilationUnit c : cus)
			constuctors.addAll(getMethodsCallingConstructor(c,calledConstructor)) ;
	    return constuctors ;
	}
	/**
	 * Returns all method declarations that in their body directly call a given constructor.
	 */
	public static List<MethodDeclaration> getMethodsCallingConstructor(CompilationUnit cu, IMethodBinding calledConstructor) {
		// here keep method declarations calling specified 'calledContructor'
		final ArrayList<MethodDeclaration> methods = new ArrayList<MethodDeclaration>();
		
		// give me all classes in current compilation unit
		List<TypeDeclaration> classes = getAllClasses(cu);
		
		// check each class
		for (TypeDeclaration cls : classes) {
			// give me all methods of current class
			List<MethodDeclaration> clsMethods = getDeclaredMethods(cls);
			
			// check each method in current class
			for (MethodDeclaration method : clsMethods) {
				if (AnalysisUtils.isCallingConstructor(method, calledConstructor))
					methods.add(method);
			}
		}
	
		return methods;
	}

	/**
	 * Returns all method declarations that in their body directly call a given method.
	 */
	public static List<MethodDeclaration> getMethodsCallingMethod(List<CompilationUnit> cus, IMethodBinding calledMethod) {
		ArrayList<MethodDeclaration> methods = new ArrayList<MethodDeclaration>() ;
		for (CompilationUnit c : cus)
			methods.addAll(getMethodsCallingMethod(c,calledMethod)) ;
	    return methods ;
	}
	
	/**
	 * Returns all method declarations that in their body directly call a given method.
	 */
	public static List<MethodDeclaration> getMethodsCallingMethod(CompilationUnit cu, IMethodBinding calledMethod) {
		// here keep method declarations calling specified 'calledMethod'
		final ArrayList<MethodDeclaration> methods = new ArrayList<MethodDeclaration>();
		
		// give me all classes in current compilation unit
		List<TypeDeclaration> classes = getAllClasses(cu);
		
		// check each class
		for (TypeDeclaration cls : classes) {
			// give me all methods of current class
			List<MethodDeclaration> clsMethods = getDeclaredMethods(cls);
			
			// check each method in current class
			for (MethodDeclaration method : clsMethods) {
				if (AnalysisUtils.isCallingMethod(method, calledMethod))
					methods.add(method);
			}
		}
	
		return methods;
	}

	/**
	 * Get all methods that are overriden by the given method.
	 */
	public static List<MethodDeclaration> getMethodsOverridenByMethod(List<CompilationUnit> cus, IMethodBinding method) {
		ArrayList<MethodDeclaration> methods = new ArrayList<MethodDeclaration>() ;
		for (CompilationUnit c : cus)
			methods.addAll(getMethodsOverridenByMethod(c,method)) ;
	    return methods ;
	}

	/**
	 * Get all methods that are overriden by the given method.
	 */
	public static List<MethodDeclaration> getMethodsOverridenByMethod(CompilationUnit cu, IMethodBinding method) {
		// here store all methods that @method overrides
		List<MethodDeclaration> overridenMethods = new ArrayList<MethodDeclaration>();
		
		// give me all declared methods
		List<MethodDeclaration> methods = getAllMethods(cu);
		
		// test if each method is overridden by given @method
		for (MethodDeclaration m : methods) {
			if /*(method.resolveBinding().overrides(m.resolveBinding())) */
			   (SignatureUtils.overrides(method, m.resolveBinding())) { 
				overridenMethods.add(m);
				//System.out.println(m.resolveBinding().getDeclaringClass().getName() + "." + m.getName());
			}
		}
		
		return overridenMethods;
	}
	
	/**
	 * Will return the top-most overriden method. If the method does not override any other method,
	 * then this will return null.
	 */
	public static MethodDeclaration getTopOverridenMethod(List<CompilationUnit> cus, IMethodBinding method) {
		List<MethodDeclaration> overridenMethods = getMethodsOverridenByMethod(cus,method) ;
		int k ;
		int N = overridenMethods.size() ;
		MethodDeclaration topCandidate ;
		for (k=0; k<N; k++) {
			topCandidate = overridenMethods.get(k) ;
			boolean ok = true ;
			for (int m=k+1; m<N; m++) {
				MethodDeclaration m_ = overridenMethods.get(m) ;
				if (SignatureUtils.overrides(topCandidate.resolveBinding(),m_.resolveBinding())) {
					ok = false ;
					break ;
				}
			}
			if (ok) return topCandidate ;
		}
		return null ;
	}

	/**
	 * Returns true if method calls a specified constructor;
	 * Subsequent instantiations are not taken in account, only direct 'new' calls
	 */
	public static boolean isCallingConstructor(final MethodDeclaration method, final IMethodBinding calledConstructor) {
		
		final FinalResult<Boolean> result = new FinalResult<Boolean>(false) ;
		
		method.accept(new ASTVisitor() {
			// Method invocation
			public boolean visit(ClassInstanceCreation node) {
				if (result.value) return false ;
				// direct call
				if (SignatureUtils.eqMethod(calledConstructor , node.resolveConstructorBinding()))
				   {
					result.value = true ;
					
					/*System.out.println(method.resolveBinding().getDeclaringClass().getName() + "."
							+ method.getName()
							+ " calls directly "
							+ calledMethod.resolveBinding().getDeclaringClass().getName()
							+ "."
							+ calledMethod.getName());*/
				}		
				return ! result.value ;
			}
			
			public boolean visit(ConstructorInvocation node) {
				if (result.value) return false ;
				if (SignatureUtils.eqMethod(calledConstructor , node.resolveConstructorBinding()))
				   {
					result.value = true ;
				   }
				return ! result.value ;
			}
		});
	
		return result.value ;
	}


	/**
	 * Returns true if method m1 directly calls a specified method m2. 
	 * 
	 * This feature is now SUSPENDED:
	 * Also, 
	 * if m1 directly calls m2' which is overriden by m2, then this m1 can 
	 * potentially calls m2 (via dynamic binding). We then conservatively
	 * return true in that case.
	 */
	public static boolean isCallingMethod(final MethodDeclaration method, final IMethodBinding calledMethod) {
		
		//System.out.println("CHECKING "+ SignatureUtils.getFullMethodName(method)
		//		+ " --?--> "
		//		+ SignatureUtils.getFullMethodName(calledMethod)) ;
		
		// To accumulate the result:
		final FinalResult<Boolean> result = new FinalResult<Boolean>(false) ;
		
		method.accept(new ASTVisitor() {
			
			// Method invocation
			public boolean visit(MethodInvocation node) {
				assert (node != null) ;
				if (result.value) return false ;
				//System.out.println("##" + node + " --is?--> " + calledMethod) ;
				if (SignatureUtils.eqMethod(node.resolveMethodBinding(),calledMethod)) 
					result.value = true ;	
				return !result.value ;
			}
			
			// Super method invocation
			public boolean visit(SuperMethodInvocation node) {
				if (result.value) return false ;
				if (SignatureUtils.eqMethod(node.resolveMethodBinding(),calledMethod))
					result.value = true ;	
				return !result.value ;
			}
	
		});
	
		return result.value ;
	}
	
	/**
	 * Check if a method is a main method.
	 */
	public static boolean isMainMethod(MethodDeclaration m){
		IMethodBinding m_ = m.resolveBinding() ;
		if (!m_.getName().equals("main")) return false ;
		boolean isPublic = false ;
		boolean isStatic = false ;
		for (Object modf : m.modifiers()) {
			isPublic = isPublic || ((Modifier) modf).isPublic() ;
			isStatic = isStatic || ((Modifier) modf).isStatic() ;
		}
		if (!isPublic || !isStatic) return false ;
		ITypeBinding[] argtypes = m_.getParameterTypes() ;
		if (argtypes.length!=1) return false ;
		boolean typeMatch = argtypes[0].getBinaryName().equals("[Ljava.lang.String;") ;
		//System.out.println("XX " + argtypes[0].getBinaryName()) ;
		//System.out.println("XX " + typeMatch) ;
        return typeMatch ;
	}
	
	
	public static boolean isAbstract(MethodDeclaration m){
		assert (m != null) ;
		for (Object modifier_ : m.modifiers()) {
			if (modifier_ instanceof Modifier) {
				if (((Modifier) modifier_).isAbstract()) return true ;
			}
		}
		return false ;
	}
	
}