/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.AbstractEnumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Model Visit Strategy Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getModelVisitStrategyType()
 * @model extendedMetaData="name='ModelVisitStrategyType'"
 * @generated
 */
public final class ModelVisitStrategyType extends AbstractEnumerator {
	/**
	 * The '<em><b>VISITORBREADTHFIRST</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORBREADTHFIRST</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRST_LITERAL
	 * @model literal="VISITOR_BREADTHFIRST"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORBREADTHFIRST = 0;

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHGLOBALLOOPS</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORBREADTHFIRSTWITHGLOBALLOOPS</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHGLOBALLOOPS_LITERAL
	 * @model literal="VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORBREADTHFIRSTWITHGLOBALLOOPS = 1;

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHLOCALLOOPS</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORBREADTHFIRSTWITHLOCALLOOPS</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHLOCALLOOPS_LITERAL
	 * @model literal="VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORBREADTHFIRSTWITHLOCALLOOPS = 2;

	/**
	 * The '<em><b>VISITORCOVERAGEUNIFORM</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORCOVERAGEUNIFORM</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORCOVERAGEUNIFORM_LITERAL
	 * @model literal="VISITOR_COVERAGE_UNIFORM"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORCOVERAGEUNIFORM = 3;

	/**
	 * The '<em><b>VISITORSEQMAXK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEQMAXK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQMAXK_LITERAL
	 * @model literal="VISITOR_SEQ_MAXK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEQMAXK = 4;

	/**
	 * The '<em><b>VISITORSEQK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEQK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQK_LITERAL
	 * @model literal="VISITOR_SEQK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEQK = 5;

	/**
	 * The '<em><b>VISITORSEMK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEMK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMK_LITERAL
	 * @model literal="VISITOR_SEMK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEMK = 6;

	/**
	 * The '<em><b>VISITORSEMMAXK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEMMAXK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMMAXK_LITERAL
	 * @model literal="VISITOR_SEM_MAXK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEMMAXK = 7;

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event Max K</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSE Msextractor Only Last Event Max K</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_LITERAL
	 * @model name="VISITORSEMsextractorOnlyLastEventMaxK" literal="VISITOR_SEMsextractor_onlyLastEvent_maxK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K = 8;

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event K</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSE Msextractor Only Last Event K</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_LITERAL
	 * @model name="VISITORSEMsextractorOnlyLastEventK" literal="VISITOR_SEMsextractor_onlyLastEvent_K"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K = 9;

	/**
	 * The '<em><b>VISITORALTMAXK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORALTMAXK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORALTMAXK_LITERAL
	 * @model literal="VISITOR_ALT_MAXK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORALTMAXK = 10;

	/**
	 * The '<em><b>VISITORDIVERSITY</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_LITERAL
	 * @model literal="VISITOR_DIVERSITY"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY = 11;

	/**
	 * The '<em><b>VISITORDIVERSITYTC</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYTC</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTC_LITERAL
	 * @model literal="VISITOR_DIVERSITY_TC"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYTC = 12;

	/**
	 * The '<em><b>VISITORDIVERSITYTL</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYTL</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTL_LITERAL
	 * @model literal="VISITOR_DIVERSITY_TL"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYTL = 13;

	/**
	 * The '<em><b>VISITORDIVERSITYEDM</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYEDM</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDM_LITERAL
	 * @model literal="VISITOR_DIVERSITY_EDM"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYEDM = 14;

	/**
	 * The '<em><b>VISITORDIVERSITYEDA</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYEDA</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDA_LITERAL
	 * @model literal="VISITOR_DIVERSITY_EDA"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYEDA = 15;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TC</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event TC</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TC_LITERAL
	 * @model name="VISITORDIVERSITYOnlyLastEventTC" literal="VISITOR_DIVERSITY_onlyLastEvent_TC"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_TC = 16;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TL</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event TL</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TL_LITERAL
	 * @model name="VISITORDIVERSITYOnlyLastEventTL" literal="VISITOR_DIVERSITY_onlyLastEvent_TL"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_TL = 17;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDA</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event EDA</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_LITERAL
	 * @model name="VISITORDIVERSITYOnlyLastEventEDA" literal="VISITOR_DIVERSITY_onlyLastEvent_EDA"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_EDA = 18;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDM</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event EDM</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_LITERAL
	 * @model name="VISITORDIVERSITYOnlyLastEventEDM" literal="VISITOR_DIVERSITY_onlyLastEvent_EDM"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_EDM = 19;

	/**
	 * The '<em><b>VISITORBREADTHFIRST</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRST
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORBREADTHFIRST_LITERAL = new ModelVisitStrategyType(VISITORBREADTHFIRST, "VISITORBREADTHFIRST", "VISITOR_BREADTHFIRST");

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHGLOBALLOOPS</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHGLOBALLOOPS
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORBREADTHFIRSTWITHGLOBALLOOPS_LITERAL = new ModelVisitStrategyType(VISITORBREADTHFIRSTWITHGLOBALLOOPS, "VISITORBREADTHFIRSTWITHGLOBALLOOPS", "VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS");

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHLOCALLOOPS</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHLOCALLOOPS
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORBREADTHFIRSTWITHLOCALLOOPS_LITERAL = new ModelVisitStrategyType(VISITORBREADTHFIRSTWITHLOCALLOOPS, "VISITORBREADTHFIRSTWITHLOCALLOOPS", "VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS");

	/**
	 * The '<em><b>VISITORCOVERAGEUNIFORM</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORCOVERAGEUNIFORM
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORCOVERAGEUNIFORM_LITERAL = new ModelVisitStrategyType(VISITORCOVERAGEUNIFORM, "VISITORCOVERAGEUNIFORM", "VISITOR_COVERAGE_UNIFORM");

	/**
	 * The '<em><b>VISITORSEQMAXK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQMAXK
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORSEQMAXK_LITERAL = new ModelVisitStrategyType(VISITORSEQMAXK, "VISITORSEQMAXK", "VISITOR_SEQ_MAXK");

	/**
	 * The '<em><b>VISITORSEQK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQK
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORSEQK_LITERAL = new ModelVisitStrategyType(VISITORSEQK, "VISITORSEQK", "VISITOR_SEQK");

	/**
	 * The '<em><b>VISITORSEMK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMK
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORSEMK_LITERAL = new ModelVisitStrategyType(VISITORSEMK, "VISITORSEMK", "VISITOR_SEMK");

	/**
	 * The '<em><b>VISITORSEMMAXK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMMAXK
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORSEMMAXK_LITERAL = new ModelVisitStrategyType(VISITORSEMMAXK, "VISITORSEMMAXK", "VISITOR_SEM_MAXK");

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event Max K</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_LITERAL = new ModelVisitStrategyType(VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K, "VISITORSEMsextractorOnlyLastEventMaxK", "VISITOR_SEMsextractor_onlyLastEvent_maxK");

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event K</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_LITERAL = new ModelVisitStrategyType(VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K, "VISITORSEMsextractorOnlyLastEventK", "VISITOR_SEMsextractor_onlyLastEvent_K");

	/**
	 * The '<em><b>VISITORALTMAXK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORALTMAXK
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORALTMAXK_LITERAL = new ModelVisitStrategyType(VISITORALTMAXK, "VISITORALTMAXK", "VISITOR_ALT_MAXK");

	/**
	 * The '<em><b>VISITORDIVERSITY</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITY_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITY, "VISITORDIVERSITY", "VISITOR_DIVERSITY");

	/**
	 * The '<em><b>VISITORDIVERSITYTC</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTC
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITYTC_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITYTC, "VISITORDIVERSITYTC", "VISITOR_DIVERSITY_TC");

	/**
	 * The '<em><b>VISITORDIVERSITYTL</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTL
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITYTL_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITYTL, "VISITORDIVERSITYTL", "VISITOR_DIVERSITY_TL");

	/**
	 * The '<em><b>VISITORDIVERSITYEDM</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDM
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITYEDM_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITYEDM, "VISITORDIVERSITYEDM", "VISITOR_DIVERSITY_EDM");

	/**
	 * The '<em><b>VISITORDIVERSITYEDA</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDA
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITYEDA_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITYEDA, "VISITORDIVERSITYEDA", "VISITOR_DIVERSITY_EDA");

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TC</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TC
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITY_ONLY_LAST_EVENT_TC_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITY_ONLY_LAST_EVENT_TC, "VISITORDIVERSITYOnlyLastEventTC", "VISITOR_DIVERSITY_onlyLastEvent_TC");

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TL</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TL
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITY_ONLY_LAST_EVENT_TL_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITY_ONLY_LAST_EVENT_TL, "VISITORDIVERSITYOnlyLastEventTL", "VISITOR_DIVERSITY_onlyLastEvent_TL");

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDA</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDA
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITY_ONLY_LAST_EVENT_EDA, "VISITORDIVERSITYOnlyLastEventEDA", "VISITOR_DIVERSITY_onlyLastEvent_EDA");

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDM</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDM
	 * @generated
	 * @ordered
	 */
	public static final ModelVisitStrategyType VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_LITERAL = new ModelVisitStrategyType(VISITORDIVERSITY_ONLY_LAST_EVENT_EDM, "VISITORDIVERSITYOnlyLastEventEDM", "VISITOR_DIVERSITY_onlyLastEvent_EDM");

	/**
	 * An array of all the '<em><b>Model Visit Strategy Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final ModelVisitStrategyType[] VALUES_ARRAY =
		new ModelVisitStrategyType[] {
			VISITORBREADTHFIRST_LITERAL,
			VISITORBREADTHFIRSTWITHGLOBALLOOPS_LITERAL,
			VISITORBREADTHFIRSTWITHLOCALLOOPS_LITERAL,
			VISITORCOVERAGEUNIFORM_LITERAL,
			VISITORSEQMAXK_LITERAL,
			VISITORSEQK_LITERAL,
			VISITORSEMK_LITERAL,
			VISITORSEMMAXK_LITERAL,
			VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_LITERAL,
			VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_LITERAL,
			VISITORALTMAXK_LITERAL,
			VISITORDIVERSITY_LITERAL,
			VISITORDIVERSITYTC_LITERAL,
			VISITORDIVERSITYTL_LITERAL,
			VISITORDIVERSITYEDM_LITERAL,
			VISITORDIVERSITYEDA_LITERAL,
			VISITORDIVERSITY_ONLY_LAST_EVENT_TC_LITERAL,
			VISITORDIVERSITY_ONLY_LAST_EVENT_TL_LITERAL,
			VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_LITERAL,
			VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_LITERAL,
		};

	/**
	 * A public read-only list of all the '<em><b>Model Visit Strategy Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Model Visit Strategy Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static ModelVisitStrategyType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			ModelVisitStrategyType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Model Visit Strategy Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static ModelVisitStrategyType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			ModelVisitStrategyType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Model Visit Strategy Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static ModelVisitStrategyType get(int value) {
		switch (value) {
			case VISITORBREADTHFIRST: return VISITORBREADTHFIRST_LITERAL;
			case VISITORBREADTHFIRSTWITHGLOBALLOOPS: return VISITORBREADTHFIRSTWITHGLOBALLOOPS_LITERAL;
			case VISITORBREADTHFIRSTWITHLOCALLOOPS: return VISITORBREADTHFIRSTWITHLOCALLOOPS_LITERAL;
			case VISITORCOVERAGEUNIFORM: return VISITORCOVERAGEUNIFORM_LITERAL;
			case VISITORSEQMAXK: return VISITORSEQMAXK_LITERAL;
			case VISITORSEQK: return VISITORSEQK_LITERAL;
			case VISITORSEMK: return VISITORSEMK_LITERAL;
			case VISITORSEMMAXK: return VISITORSEMMAXK_LITERAL;
			case VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K: return VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_LITERAL;
			case VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K: return VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_LITERAL;
			case VISITORALTMAXK: return VISITORALTMAXK_LITERAL;
			case VISITORDIVERSITY: return VISITORDIVERSITY_LITERAL;
			case VISITORDIVERSITYTC: return VISITORDIVERSITYTC_LITERAL;
			case VISITORDIVERSITYTL: return VISITORDIVERSITYTL_LITERAL;
			case VISITORDIVERSITYEDM: return VISITORDIVERSITYEDM_LITERAL;
			case VISITORDIVERSITYEDA: return VISITORDIVERSITYEDA_LITERAL;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_TC: return VISITORDIVERSITY_ONLY_LAST_EVENT_TC_LITERAL;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_TL: return VISITORDIVERSITY_ONLY_LAST_EVENT_TL_LITERAL;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_EDA: return VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_LITERAL;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_EDM: return VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_LITERAL;
		}
		return null;
	}

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private ModelVisitStrategyType(int value, String name, String literal) {
		super(value, name, literal);
	}

} //ModelVisitStrategyType
