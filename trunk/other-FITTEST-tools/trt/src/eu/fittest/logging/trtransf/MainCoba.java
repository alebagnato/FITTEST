package eu.fittest.logging.trtransf;

import java.io.* ;
import java.util.*;

import org.eclipse.jdt.core.dom.* ;
import org.eclipse.jdt.core.* ;

// Just an example to help me understand how to use JDT to access and manipulate
// AST
public class MainCoba {
	
	
	static public String getFileContent(String fileName) throws Exception {
	    File f = new File(fileName);
	    byte[] bytes = new byte[(int) f.length()];
	    InputStream in = new FileInputStream(f);
	    int m = 0, n = 0;
	    while (m < bytes.length) {
		   n = in.read(bytes, m, bytes.length - m);
		   m += n;
	    }
	    in.close();
	    return new String(bytes); 
	}
	
	public static MethodInvocation GetPushInvocation(ASTNode node) {
		// statement is log relevant => tag;
		// condition is wrapped in to Log.Push(..) call
		AST ast = node.getAST();
		MethodInvocation invocation = ast.newMethodInvocation();
		invocation.setExpression(ast.newSimpleName("Log"));
		invocation.setName(ast.newSimpleName("Push"));
		return invocation;
	}

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws Exception {
		
		String input = getFileContent("D:/workshop/projects/fittest/fittestRepos/Software/UtrechtUniv/stdprojects/sturala/v1/test/HelloWorld.java") ;
		
		ASTParser parser = ASTParser.newParser(AST.JLS3) ; // // handles JDK 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6
		parser.setSource(input.toCharArray());
		parser.setKind(ASTParser.K_COMPILATION_UNIT);
		// In order to parse 1.5 code, some compiler options need to be set to 1.5
		Map options = JavaCore.getOptions();
		JavaCore.setComplianceOptions(JavaCore.VERSION_1_5, options);
		parser.setCompilerOptions(options);
		CompilationUnit cu = (CompilationUnit) parser.createAST(null);
		
		// doing a simple code transformation, via visitor pattern:
		cu.accept(new ASTVisitor(){
			public boolean visit(IfStatement node) {
				MethodInvocation push = GetPushInvocation(node);
				Expression condition = node.getExpression();
				node.setExpression(push);
				push.arguments().add(condition);
				return true ;
			}
		}) ;
		
		System.out.println("**org code:") ;
		System.out.println(input) ;
		System.out.println("**from ast:") ;
		System.out.println(cu.toString()) ;

	}

}
