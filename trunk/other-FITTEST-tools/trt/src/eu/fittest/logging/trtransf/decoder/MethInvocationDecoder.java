package eu.fittest.logging.trtransf.decoder;

import java.util.*;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import eu.fittest.logging.trtransf.tagging.MethInvocationTagger;

import org.eclipse.jdt.core.dom.*;

public class MethInvocationDecoder {
		
	/**
	 * To construct the decoder fragment that corresponds to the given
	 * method call.
	 */
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			MethodInvocation node) {
		
		// to store the generated statements
		List<Statement> ss = new ArrayList<Statement>();
		
		AST ast = node.getAST();
		String msignature = SignatureUtils.getSignature(node.resolveMethodBinding()) ;
		String mSimpleSignature = SignatureUtils.getSimpleSignature(node.resolveMethodBinding()) ;
		
		// handle the case when the method invocation is a direct call XLog.log(...)
		if (LogRelevance.isLogCall(node)) {
			MethodInvocation inv = mkLog(orgCu,className,node) ; 			
			ss.add(ast.newExpressionStatement(inv));
			return ss;
		}
		
		// For other cases, we need to decode the arguments first, regardless of
		// whether the invoked method is log-relevant or not:
		Expression target = node.getExpression() ;
		if (target!=null)
			ss.addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, target)  ) ;	
	    List<Expression> args = node.arguments();		
		for (Expression a : args)
			ss.addAll(StmtExprDecoder.generate(rootGenerator, orgCu, className, a)  ) ;		
							
		boolean isLogRelevant = SignatureUtils.contains(rootGenerator.getLRsignatures_(),msignature) ;
		if (! isLogRelevant) {
			//System.out.println("xxx calling   " + msignature + " is NOT log relevant");
			return ss;
		}
		//System.out.println("### calling  " + msignature + " -- " + msignature + " is log relevant");
	
		// else, the called method is log-relevant
		
		//System.out.println("## invocation: " + node) ;
		String mName = node.getName().getFullyQualifiedName() ;
		List<String> variantClasses = MethInvocationTagger.calculatePolymorphicVariants(rootGenerator, node, msignature) ;
		int N = variantClasses.size() ;
		String[] variantClasses_ = new String[N] ;
		String[] variantNames = new String[N] ;
		for (int i=0; i<N; i++) {
			variantClasses_[i] = variantClasses.get(i) ;
			String variantSig = "" + variantClasses_[i] + "."  + mSimpleSignature ;
			variantNames[i] = MethodDecoder.getModifiedMethodName(rootGenerator, mName, variantSig) ;
			//System.out.println("##--- variant: " + variantNames[i]) ;
		}
		
		if (N==1) { // only one variant; so.. not polymorphic actually
			MethodInvocation callDecoder = mkCallDecoder(ast,variantClasses.get(0),variantNames[0]) ;
			ss.add(ast.newExpressionStatement(callDecoder)) ;
			return ss ;
		}
		
		// else we have multiple variants... then create a polycall 
		MethodInvocation polycall = mkPolyCall(ast,variantClasses_,variantNames) ;
		ss.add(ast.newExpressionStatement(polycall)) ;
		

		return ss ;
		
	}

	// Create a call TLog.logE()
	private static MethodInvocation mkLog(AST ast) {
		return CodeTransformUtils.mkMethodInvocation(ast,"log",ast.newSimpleName("DLog")) ;
	}
	
	private static MethodInvocation mkLog(
			CompilationUnit orgCu,
			String className,
			MethodInvocation orgCall
			) 
	{
		AST ast = orgCall.getAST() ;
		List<Expression> args = orgCall.arguments();
		
		MethodInvocation inv = mkLog(ast) ;
		String msg = ((StringLiteral) args.get(0)) . getLiteralValue() ;
		int lineNr = orgCu.getLineNumber(orgCall.getStartPosition()) ;
		String lineNrInfo = "" + className ;
		if (lineNr >=0) lineNrInfo += ":" + lineNr ;
		
		StringLiteral msg_ = ast.newStringLiteral() ;
		StringLiteral lineNrInfo_ = ast.newStringLiteral() ;
		msg_.setLiteralValue(msg) ;
		lineNrInfo_.setLiteralValue(lineNrInfo) ;
		inv.arguments().add(msg_) ;
		inv.arguments().add(lineNrInfo_) ;
		
		return inv ;
	}
	
	protected static MethodInvocation mkCallDecoder(AST ast,
			String className,
			String decoderName
			) 
	{
		return CodeTransformUtils.mkMethodInvocation(ast,decoderName,ast.newName(className)) ;
	}
	
	
	private static MethodInvocation mkPolyCall(
			AST ast,
			String[] variantClasses,
			String[] decoderNames
			)
	{
		MethodInvocation polycall = CodeTransformUtils.mkMethodInvocation(ast,"polyCall",ast.newSimpleName("DLog")) ;
		
		// create array of variant-classes:
		ArrayCreation arr1 = ast.newArrayCreation();
		arr1.setType(ast.newArrayType(ast.newSimpleType(ast.newName("Class"))));

		ArrayInitializer arrInit1 = ast.newArrayInitializer();
		for (String vc : variantClasses) {
			TypeLiteral tl = ast.newTypeLiteral();
			//System.out.println("#### variant: " + vc) ;
			tl.setType(ast.newSimpleType(ast.newName(vc)));
			arrInit1.expressions().add(tl);	
		}
		arr1.setInitializer(arrInit1);
		
		// create array of the corresponding decoders:
		ArrayCreation arr2 = ast.newArrayCreation();
		arr2.setType(ast.newArrayType(ast.newSimpleType(ast.newName("String"))));
		
		ArrayInitializer arrInit2 = ast.newArrayInitializer();
		for (String decName : decoderNames) {
			StringLiteral decName_ = ast.newStringLiteral() ;
			decName_.setLiteralValue(decName) ;
			arrInit2.expressions().add(decName_);	
		}
		arr2.setInitializer(arrInit2);
		
		polycall.arguments().add(arr1);
		polycall.arguments().add(arr2);
				
		return polycall ;
	}
	
}
