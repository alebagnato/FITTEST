package eu.fittest.logging.trtransf.normalization;

import java.util.List;

import org.eclipse.jdt.core.dom.* ;

import eu.fittest.logging.trtransf.CodeTransformer;
import eu.fittest.logging.trtransf.tagging.TaggingVisitor;

/**
 * Implement a normalization transformation:
 * 
 *    - when a new statement need to be inserted, we assumed that the insertion
 *      point is a block; but actually this may not be the case. Normalization
 *      will enclose singleton statement that can be inserted like that with a
 *      block.
 *      
 * @author Wishnu Prasetya
 *
 */
public class Normalizer extends CodeTransformer {
	
	public Normalizer(List<CompilationUnit> cus) {
		super(cus) ;
	}
	
	/**
	 * Normalize a given class.
	 */
	public CompilationUnit transform(CompilationUnit cu) {
		cu.accept(new NormalizingVisitor(cu,this)) ;
		return null ;
	}
	
	  	
}
