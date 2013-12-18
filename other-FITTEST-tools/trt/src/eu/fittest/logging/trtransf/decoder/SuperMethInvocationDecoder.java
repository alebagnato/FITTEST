package eu.fittest.logging.trtransf.decoder;

import java.util.*;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import eu.fittest.logging.trtransf.tagging.MethInvocationTagger;
import org.eclipse.jdt.core.dom.*;


public class SuperMethInvocationDecoder {
	
	/**
	 * To construct the decoder fragment that corresponds to the given
	 * method call.
	 */
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			SuperMethodInvocation node) {
		
		// to store the generated statements
		List<Statement> ss = new ArrayList<Statement>();
				
		AST ast = node.getAST();
		String msignature = SignatureUtils.getSignature(node.resolveMethodBinding()) ;
		String mSimpleSignature = SignatureUtils.getSimpleSignature(node.resolveMethodBinding()) ;
				
				
		// We need to decode the arguments first, regardless of
		// whether the invoked method is log-relevant or not:
		List<Expression> args = node.arguments();		
		for (Expression a : args)
			ss.addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, a)  ) ;		
									
		boolean isLogRelevant = SignatureUtils.contains(rootGenerator.getLRsignatures_(),msignature) ;
		if (! isLogRelevant) return ss;
			
		// else, the called method is log-relevant
		// A super-invocation is statically bound; so, no polymorphic issue there.
		// Consequently, we don't generate a polycall either.
				
		String superClassName = node.resolveMethodBinding().getDeclaringClass().getQualifiedName() ;
		String mName = node.getName().getFullyQualifiedName() ;
		String decoderName = MethodDecoder.getModifiedMethodName(rootGenerator, mName, msignature) ;
		MethodInvocation callDecoder = MethInvocationDecoder.mkCallDecoder(ast,superClassName,decoderName) ;
		ss.add(ast.newExpressionStatement(callDecoder)) ;
		
		return ss ;
	}

}
