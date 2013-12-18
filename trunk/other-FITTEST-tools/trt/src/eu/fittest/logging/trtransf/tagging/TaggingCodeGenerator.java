package eu.fittest.logging.trtransf.tagging;

import java.util.*;
import org.eclipse.jdt.core.dom.*;
import eu.fittest.logging.trtransf.classAnalysis.BitGenerator;
import eu.fittest.logging.trtransf.classAnalysis.ExceptionTypeMapper;
import eu.fittest.logging.trtransf.classAnalysis.AnalysisUtils ;
import eu.fittest.logging.trtransf.classAnalysis.LogRelevance;
import eu.fittest.logging.trtransf.classAnalysis.MethodMapper;
import eu.fittest.logging.trtransf.classAnalysis.SignatureUtils;
import eu.fittest.logging.trtransf.CodeTransformUtils; 
import eu.fittest.logging.trtransf.CodeTransformer ;

// ### FIXME import eu.fittest.logging.trtransf.exceptionHandling.ExceptionTagging;

/**
 * Define the tagging transformation. The transformation logic is actually defined
 * in the corresponding visitor class. This class mainly provides the API to the
 * transformation and setup the context for the visitor.
 * 
 * @author Ales Sturala, Wishnu Prasetya
 *
 */
public class TaggingCodeGenerator extends CodeTransformer {
		
	protected int loopIdVariableGenerator = 0 ;
	

	public TaggingCodeGenerator(List<CompilationUnit> cus) {
		super(cus) ;
	}
	
	/**
	 * Tag a given class.
	 */
	public CompilationUnit transform(CompilationUnit cu) {
		
		CodeTransformUtils.addImport(cu,"eu.fittest.tloglib.TLog") ;
		
		// Do the tagging transformation:
		cu.accept(new TaggingVisitor(cu,this)) ;
		
		// add try-catch to entry point of the application
		//  ... PENDING
			
		// insert method mapping into the code
		// WHY?? ### FIXME
		//methodMapper.InsertMethodMapping(cu);
		
		// ### FIXME exTagger.TagEntryPoint(cu);
		
		return null ;
	}
	

}
