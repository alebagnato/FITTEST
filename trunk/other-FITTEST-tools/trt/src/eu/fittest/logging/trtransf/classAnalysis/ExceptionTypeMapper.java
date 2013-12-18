package eu.fittest.logging.trtransf.classAnalysis;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier.ModifierKeyword;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/**
 * Provide ability to get the names of all exceptions that are handled in
 * a log-relevant try-catch.
 */
public class ExceptionTypeMapper {
	
	/**
	 * Will contain the names of all xceptions that are handled in
     * a log-relevant try-catch in the given compilation unit.
	 */
	final List<String> exceptionTypes;
	
	public ExceptionTypeMapper(final CompilationUnit cu, final List<MethodDeclaration> logRelevantMethods) {
		// go through exception types and make list
		exceptionTypes = GenerateUniqueHandledExceptionTypes(logRelevantMethods, cu);
	}
	
	/**
	 * Get exception types
	 * @return
	 */
	public List<String> getExceptionTypes() {
		return this.exceptionTypes;
	}
	
	/**
	 * Creates method 'getMethodIdsMapping()' for mapping of method names
	 * @param cu
	 * 
	 * WHY??
	 */
	@SuppressWarnings("unchecked")
	public void InsertMethodMapping__renaming___(CompilationUnit cu) {
		AST ast = cu.getAST();
		
		// generate a method that returns a mapping from present methods to their IDs
		MethodDeclaration m = ast.newMethodDeclaration();
		m.setName(ast.newSimpleName("getExceptionTypesMapping"));
		m.modifiers().add(ast.newModifier(ModifierKeyword.PUBLIC_KEYWORD));
		m.modifiers().add(ast.newModifier(ModifierKeyword.STATIC_KEYWORD));
		
		// set return type 'java.util.List<String[]>'
		SimpleType type = ast.newSimpleType(ast.newQualifiedName(ast.newQualifiedName(ast.newSimpleName("java"), ast.newSimpleName("util")), ast.newSimpleName("List")));
		ParameterizedType ptype = ast.newParameterizedType(type);
		ptype.typeArguments().add(ast.newArrayType(ast.newSimpleType(ast.newSimpleName("String"))));
		m.setReturnType2(ptype);

		// set body
		Block mBody = ast.newBlock();
	
		// LIST INITIALIZATION (first line)
		// create 'new java.util.ArrayList<String[]>()'
		ClassInstanceCreation newInstance = ast.newClassInstanceCreation();
		SimpleType newType = ast.newSimpleType(ast.newQualifiedName(ast.newQualifiedName(ast.newSimpleName("java"), ast.newSimpleName("util")), ast.newSimpleName("ArrayList")));
		ParameterizedType newPType = ast.newParameterizedType(newType);
		newPType.typeArguments().add(ast.newArrayType(ast.newSimpleType(ast.newSimpleName("String"))));
		newInstance.setType(newPType);
				
		// create 'java.util.List<String[]> mapping'
		VariableDeclarationFragment declFragment = ast.newVariableDeclarationFragment();
		declFragment.setName(ast.newSimpleName("mapping"));
		SimpleType declType = ast.newSimpleType(ast.newQualifiedName(ast.newQualifiedName(ast.newSimpleName("java"), ast.newSimpleName("util")), ast.newSimpleName("List")));
		ParameterizedType pDeclType = ast.newParameterizedType(declType);
		pDeclType.typeArguments().add(ast.newArrayType(ast.newSimpleType(ast.newSimpleName("String"))));
		
		// combine fragments to get 'java.util.List<String[]> mapping = new java.util.ArrayList<String[]>();'
		declFragment.setInitializer(newInstance);
		VariableDeclarationStatement declaration = ast.newVariableDeclarationStatement(declFragment);
		declaration.setType(pDeclType);
		mBody.statements().add(declaration);
		
		// ADD MAPPINGS (middle)
		for (String exceptionType : this.exceptionTypes) {
			MethodInvocation add = ast.newMethodInvocation();
			add.setExpression(ast.newSimpleName("mapping"));
			add.setName(ast.newSimpleName("add"));

			// create 'new String[] { "ArgumentException", "1" }'
			ArrayCreation arr = ast.newArrayCreation();
			arr.setType(ast.newArrayType(ast.newSimpleType(ast.newSimpleName("String"))));
			ArrayInitializer ai = ast.newArrayInitializer();

			StringLiteral exceptionTypeNode = ast.newStringLiteral();
			StringLiteral idNode = ast.newStringLiteral();

			exceptionTypeNode.setLiteralValue(exceptionType);
			idNode.setLiteralValue(this.GetExceptionTypeBits(exceptionType));

			ai.expressions().add(exceptionTypeNode);
			ai.expressions().add(idNode);

			arr.setInitializer(ai);
			add.arguments().add(arr);
			ExpressionStatement addStatement = ast.newExpressionStatement(add);
			
			mBody.statements().add(addStatement);
		}

		// RETURN MAPPING (last line)
		// generate 'return' statement
		ReturnStatement rtrn = ast.newReturnStatement();
		rtrn.setExpression(ast.newSimpleName("mapping"));
		
		mBody.statements().add(rtrn);
		m.setBody(mBody);
		
		((TypeDeclaration)cu.types().get(0)).bodyDeclarations().add(m);
	}	

	/**
	 * Goes through all log relevant try-catch blocks and gathers handled exception types
	 * in catch clauses
	 * @param node
	 * @return
	 */
	static List<String> GenerateUniqueHandledExceptionTypes(final List<MethodDeclaration> logRelevantMethods, final CompilationUnit cu) {
		
		final ArrayList<String> handledExceptionTypes = new ArrayList<String>();
		
		// find all catch clauses
		cu.accept(new ASTVisitor() {
			@SuppressWarnings("unchecked")
			public boolean visit(TryStatement node) {							// TRY-CATCH
				
				// check if node is log relevant
				Boolean isLogRelevan = false ; // FIX THIS! LogRelevance.isLogRelevantNode(node, logRelevantMethods, cu);
				if (1==1) throw new Error("FIXME") ;
				if (!isLogRelevan)
					return false;
				
				List<CatchClause> catchBlocks = node.catchClauses();
				for (CatchClause c : catchBlocks) {
					String exceptionType = ((SimpleType)c.getException().getType()).getName().getFullyQualifiedName();

					boolean exists = false;
					
					// if exception type already is in the list => go to next catch block
					for (String t : handledExceptionTypes) {
						if (t.equals(exceptionType)) {
							exists = true;
							break ; 
						}
					}
					
					if (!exists)
						handledExceptionTypes.add(exceptionType);
				}
				
				return true;
			}
		});
	
		return handledExceptionTypes;
	}

	/**
	 * Each exception type handled by a log relevant try-catch statement is assigned
	 * a unique fixed-length combination of bits; this bit combination is saved in event log if needed
	 * to represent the event type; If a given exception type is not among exception types that
	 * are handled by log relevant try-catch blocks, then this exception type is not handled in
	 * a log code and exception type is represented with only fixed-length 0
	 * @param cu
	 * @return
	 */
	public String GetExceptionTypeBits(String exceptionType) {
		// get bit fixed-length
		int size = BitGenerator.countBitsCombination(exceptionTypes.size() + 1); // +1 represents the exception types that are not represented with a unique bit combination
		BitGenerator bitGenerator = new BitGenerator(size);
		String unhandled = bitGenerator.next();			// unhadled exception in log code are represented with a single bit combination
		for (String t : exceptionTypes) {
			
			if (t.equals(exceptionType))
				return bitGenerator.next();
			else
				bitGenerator.next();
		}
		
		// unhandled exception (not found among handled)
		return unhandled;
	}
}
