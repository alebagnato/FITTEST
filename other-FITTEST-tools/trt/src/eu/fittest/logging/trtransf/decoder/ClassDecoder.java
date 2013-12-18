package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.* ;
import java.util.List;
import org.eclipse.jdt.core.dom.*;


public class ClassDecoder {
	
	/**
	 * Generate the decoder of a class (could be an inner class).
	 * 
	 * @rootGenerator A pointer to the original Decode-generator. This is needed because
	 *                this decoder maintains a set of useful global infos, such as
	 *                the list of log-relevant methods.
	 *                
	 * @cu The original compilation unit containing the oldType. This is needed to
	 *     later get line-numbers information.
	 */	
	protected static TypeDeclaration generate(
			CodeTransformer rootGenerator, 
			CompilationUnit cu,
			TypeDeclaration oldType) 
	{
		
		AST ast = oldType.getAST();
		TypeDeclaration newType = ast.newTypeDeclaration();
		String oldName = oldType.getName().getFullyQualifiedName() ;
		newType.setName(ast.newSimpleName(oldName));
		
		// copy modifiers;
		// we make all classes public so if private modifier found skip it,
		// and add public modifier
		List<Modifier> modifiers = oldType.modifiers();
		boolean hasPublicKeyword = false;
		for (Modifier modifier : modifiers) {
			if (modifier.isPrivate() || modifier.isProtected()) continue;			
			if (modifier.isPublic()) hasPublicKeyword = true;			
			newType.modifiers().add(ASTNode.copySubtree(ast, modifier));
		}
		// if class is not public => add public keyword
		if (!hasPublicKeyword)
			newType.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
		
		// first, insert a helper constructor into the class that is being inspected,
		// to allow us to create an instance of the class at any time:	
		// NOTE: removed... not needed in the new version!
		// insertConstructor1(newType, oldType) ;
				
		// if class extends a class => set 'extends'
		if (oldType.getSuperclassType() != null) {
			String n = ((SimpleType)oldType.getSuperclassType()).getName().getFullyQualifiedName();			// we expect always SimpleType is used
			newType.setSuperclassType(ast.newSimpleType(ast.newName(n)));
		}
		
		// now, for every method or inner-class of this class, we generate
		// its decoder:
		List<MethodDeclaration> bodyDecls = oldType.bodyDeclarations() ;		
		for (Object d : bodyDecls) {
			if (d instanceof TypeDeclaration) {
				TypeDeclaration c = generate(rootGenerator,cu,(TypeDeclaration) d);
				if (c != null)
					newType.bodyDeclarations().add(c) ;
			} 
			else if (d instanceof MethodDeclaration) {
				String typeName = oldType.getName().getFullyQualifiedName() ;
				MethodDeclaration m = MethodDecoder.generate(rootGenerator,cu,typeName,(MethodDeclaration)d);
				if (m != null)
					newType.bodyDeclarations().add(m);
			}
		}
		
		// OLD:
		// for each exception type handled in log code store mapping between 
		// the exception type and its unique id
		// Q: dont know yet if this is needed in the new design
		//
		//ExceptionTypeMapper exceptionTypeMapper = new ExceptionTypeMapper(cu, logRelevantMethods);
		//exceptionTypeMapper.InsertMethodMapping(lcu);
		
		return newType ;
	}
	
	
	private static String[] splitName(String name) {
		String[] split = new String[2] ;
		int k = name.length() ;
		while (0<k) {
			k-- ;
			if (name.charAt(k) == '.') break ;
		}
		if (name.charAt(k) == '.') {
			split[0] = "" + name.substring(0,k) ;
			split[1] = "" + name.substring(k+1,name.length()) ;		
		}
		else {
			split[0] = null ;
			split[1] = "" + name ;
		}
		return split ;
	}
		
	/**
	 * This is used to insert a helper constructor to the class that is being
	 * inspected.
	 */
	private static void insertConstructor1(TypeDeclaration newType, TypeDeclaration oldType) {	
		AST ast = newType.getAST() ;
		MethodDeclaration cm = ast.newMethodDeclaration();
		cm.setConstructor(true);
		cm.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
		cm.setName(ast.newSimpleName(newType.getName().getFullyQualifiedName()));
		
		// set the constructor's parameter
		SingleVariableDeclaration p = ast.newSingleVariableDeclaration();
		p.setName(ast.newSimpleName("c"));
		p.setType(ast.newSimpleType(ast.newName("Boolean")));
		cm.parameters().add(p);
		
		// set contructor's body;
		// if node is subclass of another class, we have to call in the new
		// constructor also new constructor of the ancestor's class: super(true);
		if (oldType.getSuperclassType() != null) {
			SuperConstructorInvocation ci = ast.newSuperConstructorInvocation();
			ci.arguments().add(ast.newBooleanLiteral(true));
					
			Block cmb = ast.newBlock();
			cmb.statements().add(ci);
			cm.setBody(cmb);
		} 
		else // class does not extend another class => set empty body of the new constructor
		{
			cm.setBody(ast.newBlock());
		}
		newType.bodyDeclarations().add(cm) ;
	}

}
