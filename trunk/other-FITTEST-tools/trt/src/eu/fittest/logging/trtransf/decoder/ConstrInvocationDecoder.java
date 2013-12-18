package eu.fittest.logging.trtransf.decoder;

import java.util.*;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import eu.fittest.logging.trtransf.tagging.MethInvocationTagger;

import org.eclipse.jdt.core.dom.*;

public class ConstrInvocationDecoder {
	
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			ConstructorInvocation node) {
				
		// to store the generated statements
		List<Statement> ss = new ArrayList<Statement>();
				
		AST ast = node.getAST();
		String msignature = SignatureUtils.getSignature(node.resolveConstructorBinding()) ;
	    String mName = node.resolveConstructorBinding().getName() ;		
				
		// We need to decode the arguments first, regardless of
		// whether the invoked method is log-relevant or not:
		List<Expression> args = node.arguments();		
		for (Expression a : args)
			ss.addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, a)  ) ;		
									
		boolean isLogRelevant = SignatureUtils.contains(rootGenerator.getLRsignatures_(),msignature) ;
		if (! isLogRelevant) {
			//System.out.println("### " + mName + " -- " + msignature + " is NOT log relevant");
			return ss;
		}
		//System.out.println("### " + mName + " -- " + msignature + " is log relevant");
			
		
		String decoderName = MethodDecoder.getModifiedMethodName(rootGenerator, mName, msignature) ;
		MethodInvocation callDecoder = MethInvocationDecoder.mkCallDecoder(ast,className,decoderName) ;
		ss.add(ast.newExpressionStatement(callDecoder)) ;
		
		return ss ;
		
	}

}
