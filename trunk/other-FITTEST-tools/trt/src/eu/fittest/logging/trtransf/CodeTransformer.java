package eu.fittest.logging.trtransf;

import java.util.*;

import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;

import eu.fittest.logging.trtransf.classAnalysis.*;


/**
 * An abstract super class of our transformations. It holds the context information
 * common to various kinds of transformations (normalization, tagging, generating decoders).
 * 
 * @author Wishnu Prasetya
 *
 */
public class CodeTransformer {
	
	/**
	 * Provide here a list of log-relevant methods.
	 */
	protected List<MethodDeclaration> logRelevantMethods ;
	
	/**
	 * Provide here a list of all declared methods.
	 */
	protected List<MethodDeclaration> allMethods ;
		
	/**
	 * A mapping from method declarations of log-relevant methods to
	 * their signature (so that we don't have to keep calculating them).
	 */
	protected Map<MethodDeclaration,String> lrSignatures ;
	
	/**
	 * Reverse mapping of lrSignatures.
	 */
	protected Map<String,MethodDeclaration> lrSignaturesInv ;
	/**
	 * List of all signatures of log-relevant methods.
	 */
	protected List<String> lrSignatures_ ;
	
	/**
	 * A mapping from ALL method declarations to
	 * their signature (so that we don't have to keep calculating them).
	 */
	protected Map<MethodDeclaration,String> signatures ;
	
	/**
	 * Reverse mapping of signatures.
	 */
	protected Map<String,MethodDeclaration> signaturesInv ;
	/**
	 * List of all signatures.
	 */
	protected List<String> signatures_ ;
	
	protected Map<String,List<MethodDeclaration>> overridens  ;
	protected Map<String,List<MethodDeclaration>> overridings ;
	
	protected  Map<MethodDeclaration,Integer> uniqueIDmap ;
	protected  Map<String,Integer> uniqueIDmap_ ;
	
	/**
	 * This is the list of target classes to tag.
	 */
	protected List<CompilationUnit> cus ;
	
	/**
	 * Construct a transformer; this will set up various default context
	 * data for the transformer.
	 */
	public CodeTransformer(List<CompilationUnit> cus) {
		this.cus = cus ;
		logRelevantMethods = LogRelevance.getLogRelevantMethods(cus) ;
		allMethods = AnalysisUtils.getAllMethods(cus) ;
		lrSignatures    = new HashMap<MethodDeclaration,String>() ;
		lrSignaturesInv = new HashMap<String,MethodDeclaration>() ;
		lrSignatures_   = new ArrayList<String>() ;
		signatures    = new HashMap<MethodDeclaration,String>() ;
		signaturesInv = new HashMap<String,MethodDeclaration>() ;
		signatures_   = new ArrayList<String>() ;
		overridens    = new HashMap<String,List<MethodDeclaration>>() ;
		overridings   = new HashMap<String,List<MethodDeclaration>>() ;
		uniqueIDmap   = new HashMap<MethodDeclaration,Integer>() ;
		uniqueIDmap_   = new HashMap<String,Integer>() ;
		
		for (MethodDeclaration m : logRelevantMethods) {
			IMethodBinding mb = m.resolveBinding() ;
			String sig = SignatureUtils.getSignature(mb) ;
			lrSignatures.put(m,sig) ;
			lrSignaturesInv.put(sig,m) ;
			lrSignatures_.add(sig) ;
		}
		
		int uniqueID = 0 ;
		for (MethodDeclaration m : allMethods) {
			IMethodBinding mb = m.resolveBinding() ;
			String sig = SignatureUtils.getSignature(mb) ;
			signatures.put(m,sig) ;
			signaturesInv.put(sig,m) ;
			signatures_.add(sig) ;
			overridens.put(sig, AnalysisUtils.getMethodsOverridenByMethod(cus,mb)) ;
			overridings.put(sig, AnalysisUtils.getOverridingMethods(cus,mb)) ;
			
			// assigning unique IDs to methods
			uniqueIDmap.put(m,uniqueID) ;
			uniqueIDmap_.put(sig,uniqueID) ;
			uniqueID++ ;
		}
	}
	
	
	/**
	 * Transform all classes in cus, one at a time. This either apply the
	 * transformation as side effect on cus, or produce a new list of 
	 * compilation units.
	 */
	public List<CompilationUnit> transform() {
		List<CompilationUnit> result = new LinkedList<CompilationUnit>() ;
		for (CompilationUnit c : cus) {
			CompilationUnit r = transform(c) ;
			result.add(r) ;
		}
		return result ;
	} 
	
	/**
	 * Transform a single given class. It either apply the transformation 
	 * directly (side effect) on the given cu, or it produces a new 
	 * compilation unit.
	 */
	public CompilationUnit transform(CompilationUnit cu) { 
		throw new UnsupportedOperationException() ;
	}
	
	// various getters:
	
	public List<MethodDeclaration> getLogRelevantMethods() {
		return logRelevantMethods;
	}

	public List<MethodDeclaration> getAllMethods() {
		return allMethods;
	}
	
	public Map<MethodDeclaration, String> getLRsignatures() {
		return lrSignatures;
	}

	public Map<String, MethodDeclaration> getLRsignaturesInv() {
		return lrSignaturesInv;
	}

	public List<String> getLRsignatures_() {
		return lrSignatures_;
	}
	
	public Map<MethodDeclaration, String> getSignatures() {
		return signatures;
	}

	public Map<String, MethodDeclaration> getSignaturesInv() {
		return signaturesInv;
	}

	public List<String> getSignatures_() {
		return signatures_;
	}

	public Map<MethodDeclaration, Integer> getUniqueIDmap() {
		return uniqueIDmap;
	}
	
	public Map<String, Integer> getUniqueIDmap_() {
		return uniqueIDmap_;
	}
	
	public Map<String, List<MethodDeclaration>> getOverridens() {
		return overridens;
	}

	public Map<String, List<MethodDeclaration>> getOverridings() {
		return overridings;
	}

	public List<CompilationUnit> getCus() {
		return cus;
	}
	

}
