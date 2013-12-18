package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.* ;
import eu.fittest.logging.trtransf.classAnalysis.*;
import java.util.*;
import org.eclipse.jdt.core.dom.*;
import org.eclipse.jdt.core.dom.Modifier.ModifierKeyword;

public class MethodDecoder {
	
	
	/**
	 * Generate the decoder of a method or constructor.
	 */	
	protected static MethodDeclaration generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String orgClassName,
			MethodDeclaration oldM) {
		
		if (AnalysisUtils.isAbstract(oldM)) // don't bother 
			return null ;
		
		AST ast = oldM.getAST();
		String oldMsignature = rootGenerator.getSignatures().get(oldM) ;
		String oldMname = oldM.getName().getFullyQualifiedName() ;
		MethodDeclaration newM = ast.newMethodDeclaration();
		
		// check if the method is log-relevant, or it is overriden by a log-relevant
		// method:
		Boolean isLogRelevan = rootGenerator.getLogRelevantMethods().contains(oldM) ;
				
		// The decoder of this method will have the same name, but will have no
		// parameters. However, then we need to handle overloading. So, we
        // rename the method, assigning a unique number to it, to distinguish
		// between different overloading cases.
		String newMname = getModifiedMethodName(rootGenerator,oldMname,oldMsignature) ;
		newM.setName(ast.newSimpleName(newMname));
		// they all will be made non-constructor...
		newM.setConstructor(false) ;
		
		// setting modifiers
		//List modifiers0 = ASTNode.copySubtrees(newAst, oldM.modifiers()) ;
		//newM.modifiers().addAll(modifiers0)  ;
		makeItStatic(newM) ;
		makeItPublic(newM) ;
		
		// setting exception
		Name exc = ast.newSimpleName("Exception") ;
		newM.thrownExceptions().add(exc) ;
		
		
		addComment(rootGenerator,newM,oldM) ;
		
		Block oldBody = oldM.getBody();
		
		if (isLogRelevan) {
			// Note that a method overriden by a log-relevant method is also considered
			// as logrelevant
			newM.setBody(newM.getAST().newBlock()) ;
			List<Statement> statements  = oldBody.statements() ;
			//assert statements != null ;
			List<Statement> ss = new ArrayList<Statement>() ;
			for (Statement s : statements) {
			  ss.addAll(StmtExprDecoder.generate(rootGenerator,orgCu,orgClassName,s));
			}
			newM.getBody().statements().addAll(ss);	
		}
		else {
			// If it is not-log relevant, but it is overriding a log-relevant method
			// we need to generate an skip-decoder for it as part of dealing with
			// polymorphism
			List<MethodDeclaration> overridens = rootGenerator.getOverridens().get(oldMsignature) ;
			boolean overridingLogRelevant = false ;
			for (MethodDeclaration k : overridens) {
				if (rootGenerator.getLogRelevantMethods().contains(k)) {
					overridingLogRelevant = true ;
					break ;
				}
			}
			if (!overridingLogRelevant) {
				// then don't bother, no need to generate a decoder 
				return null ;
			}
			else {
				// make a decoder with an empty body
				newM.setBody(newM.getAST().newBlock()) ;
			}		
		}
				
		return newM ;
	}
	
	
	public static String getModifiedMethodName(CodeTransformer context,
			String originalName,
			String signature) {
		int methodUniqueNr = context.getUniqueIDmap_().get(signature) ;
		return "dec_" + originalName + methodUniqueNr ;
	}
	
	private static void makeItStatic(MethodDeclaration m) {
		// don't know how to unset the abstract modifier; so I'll do this now
		// just by adding an empty body to m
		/*
		boolean isStatic = false ;
		for (Object modifier_ : m.modifiers()) {
			if (modifier_ instanceof Modifier) {
				if (((Modifier) modifier_).isStatic()) {
					isStatic = true ; break ;
				}
			}
		}
		if (!isStatic )
		*/
	     m.modifiers().add(m.getAST().newModifier(Modifier.ModifierKeyword.STATIC_KEYWORD));
	}
	
	private static void makeItPublic(MethodDeclaration m) {
		// don't know how to unset the abstract modifier; so I'll do this now
		// just by adding an empty body to m
		/*
		boolean isPublic = false ;
		for (Object modifier_ : m.modifiers()) {
			if (modifier_ instanceof Modifier) {
				if (((Modifier) modifier_).isPublic()) {
					isPublic = true ; break ;
				}
			}
		}
		if (!isPublic )
		*/
		m.modifiers().add(m.getAST().newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
	}
	
	private static void addComment(CodeTransformer rootGenerator, 
			MethodDeclaration newM, 
			MethodDeclaration oldM) 
	{
		AST newAst = newM.getAST() ;
		String oldMsignature = rootGenerator.getSignatures().get(oldM) ;
		Boolean isLogRelevan = rootGenerator.getLogRelevantMethods().contains(oldM) ;
		String comment = "" ;
		if (oldM.isConstructor()) comment += "(C) " ;
		comment += oldMsignature ;
		if (isLogRelevan) comment += " <<LR>>" ;
		Javadoc jdoc = newAst.newJavadoc() ;
		TagElement jdoctagelem = newAst.newTagElement() ;
		TextElement comment_ = newAst.newTextElement() ;
		comment_.setText(comment) ;
		jdoctagelem.fragments().add(comment_) ;
		jdoc.tags().add(jdoctagelem) ;
		newM.setJavadoc(jdoc) ;
	}

}
