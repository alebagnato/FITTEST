package eu.fittest.logging.trtransf.classAnalysis;

import java.util.*;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.Modifier.ModifierKeyword;

/**
 * Provide ability to construct a map, mapping each log relevant method to a unique ID.
 * 
 */
public class MethodMapper {
	
	/**
	 * A mapping from method signatures to a unique ID.
	 */
	public Map<String,Integer> methodIdsMapper ;
	
	public MethodMapper(List<String> logRelevantMethods) {
		methodIdsMapper = new HashMap<String,Integer>() ;
		int id = 0 ;
		for (String m : logRelevantMethods) {
			methodIdsMapper.put(m,id) ; id++ ;
		}			
	}

	
	/**
	 * For method with given signature returns its assigned unique ID.
	 * @param fileName
	 * @param className
	 * @param methodName
	 * @return
	 */
	public int getMethodId(String methodSignature) {
		Integer id = methodIdsMapper.get(methodSignature) ;
		if (id!=null) return id ;
		throw new Error("Method ID for corresponding signature "+ methodSignature + " is not found!") ;
	}
	
	/**
	 * Creates method 'getMethodIdsMapping()' for mapping of method names.
	 * @param cu
	 * 
	 * WHAT IS THIS FOR??
	 */
	// @SuppressWarnings("unchecked")
	/*
	public void insertMethodMapping___remaningthis__(CompilationUnit cu) {
		AST ast = cu.getAST();
		
		// generate a method that returns a mapping from present methods to their IDs
		MethodDeclaration m = ast.newMethodDeclaration();
		m.setName(ast.newSimpleName("getMethodIdsMapping"));
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
		List<String[]> methodMapping = this.getMapping();
		for (String[] mapping : methodMapping) {
			MethodInvocation add = ast.newMethodInvocation();
			add.setExpression(ast.newSimpleName("mapping"));
			add.setName(ast.newSimpleName("add"));

			// create 'new String[] { "a", "b", "c", "1" }'
			ArrayCreation arr = ast.newArrayCreation();
			arr.setType(ast.newArrayType(ast.newSimpleType(ast.newSimpleName("String"))));
			ArrayInitializer ai = ast.newArrayInitializer();

			StringLiteral fileNameNode = ast.newStringLiteral();
			StringLiteral classNameNode = ast.newStringLiteral();
			StringLiteral methodNameNode = ast.newStringLiteral();
			StringLiteral idNode = ast.newStringLiteral();

			fileNameNode.setLiteralValue(mapping[0]);
			classNameNode.setLiteralValue(mapping[1]);
			methodNameNode.setLiteralValue(mapping[2]);
			idNode.setLiteralValue(mapping[3]);

			ai.expressions().add(fileNameNode);
			ai.expressions().add(classNameNode);
			ai.expressions().add(methodNameNode);
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
	*/
}
