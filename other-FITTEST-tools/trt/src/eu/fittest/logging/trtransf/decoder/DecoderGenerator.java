package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.* ;
import eu.fittest.logging.trtransf.classAnalysis.* ;
import java.util.* ;
import org.eclipse.jdt.core.dom.*;



public class DecoderGenerator extends CodeTransformer {

	public DecoderGenerator(List<CompilationUnit> cus) {
		super(cus) ;
	}
	
	/**
	 * Generate the decoder of the given class.
	 */
	public CompilationUnit transform(CompilationUnit cu) {
		return generateCU(cu) ;
	} 
	
	/**
	 * Generate the decoder of a compilation unit (usually a top-level class).
	 */
	CompilationUnit generateCU(CompilationUnit cu) {

		AST ast = cu.getAST();
		CompilationUnit lcu = ast.newCompilationUnit();
		AST newAst = lcu.getAST() ;
		
		// copy package	declaration	
		if (cu.getPackage() != null) {
		   PackageDeclaration pckg0 = cu.getPackage() ;
		   PackageDeclaration pckg = (PackageDeclaration) ASTNode.copySubtree(newAst,pckg0) ;
		   lcu.setPackage(pckg);
		}
		
		// copy the import declarations
		List imports0 = ASTNode.copySubtrees(newAst, cu.imports()) ;
		lcu.imports().addAll(imports0) ;
		
		// Add this import to the decoder-lib too:
		ImportDeclaration ix = newAst.newImportDeclaration();
		ix.setName(lcu.getAST().newName("eu.fittest.tloglib.DLog"));
		lcu.imports().add(ix);
		
		// For every class in the compilation unit, generate its
		// decoder:
        List<TypeDeclaration> tys = lcu.types();	
        for (Object t0_ : cu.types()) {
        	TypeDeclaration t0 = (TypeDeclaration) t0_ ;
        	if (LogRelevance.isLogRelevantNode(t0, getLRsignatures_())) 
        		tys.add(ClassDecoder.generate(this,cu,t0)) ;
        }
        
		return lcu ;		
	}
		
}
