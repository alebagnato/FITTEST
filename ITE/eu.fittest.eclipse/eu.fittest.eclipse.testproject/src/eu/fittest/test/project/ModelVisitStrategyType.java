/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Model Visit Strategy Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getModelVisitStrategyType()
 * @model extendedMetaData="name='ModelVisitStrategyType'"
 * @generated
 */
public enum ModelVisitStrategyType implements Enumerator {
	/**
	 * The '<em><b>VISITORBREADTHFIRST</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRST_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORBREADTHFIRST(0, "VISITORBREADTHFIRST", "VISITOR_BREADTHFIRST"),

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHGLOBALLOOPS</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHGLOBALLOOPS_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORBREADTHFIRSTWITHGLOBALLOOPS(1, "VISITORBREADTHFIRSTWITHGLOBALLOOPS", "VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS"),

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHLOCALLOOPS</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHLOCALLOOPS_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORBREADTHFIRSTWITHLOCALLOOPS(2, "VISITORBREADTHFIRSTWITHLOCALLOOPS", "VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS"),

	/**
	 * The '<em><b>VISITORCOVERAGEUNIFORM</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORCOVERAGEUNIFORM_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORCOVERAGEUNIFORM(3, "VISITORCOVERAGEUNIFORM", "VISITOR_COVERAGE_UNIFORM"),

	/**
	 * The '<em><b>VISITORSEQMAXK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQMAXK_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORSEQMAXK(4, "VISITORSEQMAXK", "VISITOR_SEQ_MAXK"),

	/**
	 * The '<em><b>VISITORSEQK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQK_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORSEQK(5, "VISITORSEQK", "VISITOR_SEQK"),

	/**
	 * The '<em><b>VISITORSEMK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMK_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORSEMK(6, "VISITORSEMK", "VISITOR_SEMK"),

	/**
	 * The '<em><b>VISITORSEMMAXK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMMAXK_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORSEMMAXK(7, "VISITORSEMMAXK", "VISITOR_SEM_MAXK"),

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event Max K</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K(8, "VISITORSEMsextractorOnlyLastEventMaxK", "VISITOR_SEMsextractor_onlyLastEvent_maxK"),

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event K</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K(9, "VISITORSEMsextractorOnlyLastEventK", "VISITOR_SEMsextractor_onlyLastEvent_K"),

	/**
	 * The '<em><b>VISITORALTMAXK</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORALTMAXK_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORALTMAXK(10, "VISITORALTMAXK", "VISITOR_ALT_MAXK"),

	/**
	 * The '<em><b>VISITORDIVERSITY</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITY(11, "VISITORDIVERSITY", "VISITOR_DIVERSITY"),

	/**
	 * The '<em><b>VISITORDIVERSITYTC</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTC_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITYTC(12, "VISITORDIVERSITYTC", "VISITOR_DIVERSITY_TC"),

	/**
	 * The '<em><b>VISITORDIVERSITYTL</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTL_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITYTL(13, "VISITORDIVERSITYTL", "VISITOR_DIVERSITY_TL"),

	/**
	 * The '<em><b>VISITORDIVERSITYEDM</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDM_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITYEDM(14, "VISITORDIVERSITYEDM", "VISITOR_DIVERSITY_EDM"),

	/**
	 * The '<em><b>VISITORDIVERSITYEDA</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDA_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITYEDA(15, "VISITORDIVERSITYEDA", "VISITOR_DIVERSITY_EDA"),

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TC</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TC_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITY_ONLY_LAST_EVENT_TC(16, "VISITORDIVERSITYOnlyLastEventTC", "VISITOR_DIVERSITY_onlyLastEvent_TC"),

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TL</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TL_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITY_ONLY_LAST_EVENT_TL(17, "VISITORDIVERSITYOnlyLastEventTL", "VISITOR_DIVERSITY_onlyLastEvent_TL"),

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDA</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITY_ONLY_LAST_EVENT_EDA(18, "VISITORDIVERSITYOnlyLastEventEDA", "VISITOR_DIVERSITY_onlyLastEvent_EDA"),

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDM</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_VALUE
	 * @generated
	 * @ordered
	 */
	VISITORDIVERSITY_ONLY_LAST_EVENT_EDM(19, "VISITORDIVERSITYOnlyLastEventEDM", "VISITOR_DIVERSITY_onlyLastEvent_EDM");

	/**
	 * The '<em><b>VISITORBREADTHFIRST</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORBREADTHFIRST</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRST
	 * @model literal="VISITOR_BREADTHFIRST"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORBREADTHFIRST_VALUE = 0;

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHGLOBALLOOPS</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORBREADTHFIRSTWITHGLOBALLOOPS</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHGLOBALLOOPS
	 * @model literal="VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORBREADTHFIRSTWITHGLOBALLOOPS_VALUE = 1;

	/**
	 * The '<em><b>VISITORBREADTHFIRSTWITHLOCALLOOPS</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORBREADTHFIRSTWITHLOCALLOOPS</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORBREADTHFIRSTWITHLOCALLOOPS
	 * @model literal="VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORBREADTHFIRSTWITHLOCALLOOPS_VALUE = 2;

	/**
	 * The '<em><b>VISITORCOVERAGEUNIFORM</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORCOVERAGEUNIFORM</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORCOVERAGEUNIFORM
	 * @model literal="VISITOR_COVERAGE_UNIFORM"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORCOVERAGEUNIFORM_VALUE = 3;

	/**
	 * The '<em><b>VISITORSEQMAXK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEQMAXK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQMAXK
	 * @model literal="VISITOR_SEQ_MAXK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEQMAXK_VALUE = 4;

	/**
	 * The '<em><b>VISITORSEQK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEQK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEQK
	 * @model literal="VISITOR_SEQK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEQK_VALUE = 5;

	/**
	 * The '<em><b>VISITORSEMK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEMK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMK
	 * @model literal="VISITOR_SEMK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEMK_VALUE = 6;

	/**
	 * The '<em><b>VISITORSEMMAXK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSEMMAXK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSEMMAXK
	 * @model literal="VISITOR_SEM_MAXK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSEMMAXK_VALUE = 7;

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event Max K</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSE Msextractor Only Last Event Max K</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K
	 * @model name="VISITORSEMsextractorOnlyLastEventMaxK" literal="VISITOR_SEMsextractor_onlyLastEvent_maxK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_VALUE = 8;

	/**
	 * The '<em><b>VISITORSE Msextractor Only Last Event K</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORSE Msextractor Only Last Event K</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K
	 * @model name="VISITORSEMsextractorOnlyLastEventK" literal="VISITOR_SEMsextractor_onlyLastEvent_K"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_VALUE = 9;

	/**
	 * The '<em><b>VISITORALTMAXK</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORALTMAXK</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORALTMAXK
	 * @model literal="VISITOR_ALT_MAXK"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORALTMAXK_VALUE = 10;

	/**
	 * The '<em><b>VISITORDIVERSITY</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY
	 * @model literal="VISITOR_DIVERSITY"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_VALUE = 11;

	/**
	 * The '<em><b>VISITORDIVERSITYTC</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYTC</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTC
	 * @model literal="VISITOR_DIVERSITY_TC"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYTC_VALUE = 12;

	/**
	 * The '<em><b>VISITORDIVERSITYTL</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYTL</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYTL
	 * @model literal="VISITOR_DIVERSITY_TL"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYTL_VALUE = 13;

	/**
	 * The '<em><b>VISITORDIVERSITYEDM</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYEDM</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDM
	 * @model literal="VISITOR_DIVERSITY_EDM"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYEDM_VALUE = 14;

	/**
	 * The '<em><b>VISITORDIVERSITYEDA</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITYEDA</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITYEDA
	 * @model literal="VISITOR_DIVERSITY_EDA"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITYEDA_VALUE = 15;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TC</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event TC</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TC
	 * @model name="VISITORDIVERSITYOnlyLastEventTC" literal="VISITOR_DIVERSITY_onlyLastEvent_TC"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_TC_VALUE = 16;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event TL</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event TL</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_TL
	 * @model name="VISITORDIVERSITYOnlyLastEventTL" literal="VISITOR_DIVERSITY_onlyLastEvent_TL"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_TL_VALUE = 17;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDA</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event EDA</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDA
	 * @model name="VISITORDIVERSITYOnlyLastEventEDA" literal="VISITOR_DIVERSITY_onlyLastEvent_EDA"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_VALUE = 18;

	/**
	 * The '<em><b>VISITORDIVERSITY Only Last Event EDM</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VISITORDIVERSITY Only Last Event EDM</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #VISITORDIVERSITY_ONLY_LAST_EVENT_EDM
	 * @model name="VISITORDIVERSITYOnlyLastEventEDM" literal="VISITOR_DIVERSITY_onlyLastEvent_EDM"
	 * @generated
	 * @ordered
	 */
	public static final int VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_VALUE = 19;

	/**
	 * An array of all the '<em><b>Model Visit Strategy Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final ModelVisitStrategyType[] VALUES_ARRAY =
		new ModelVisitStrategyType[] {
			VISITORBREADTHFIRST,
			VISITORBREADTHFIRSTWITHGLOBALLOOPS,
			VISITORBREADTHFIRSTWITHLOCALLOOPS,
			VISITORCOVERAGEUNIFORM,
			VISITORSEQMAXK,
			VISITORSEQK,
			VISITORSEMK,
			VISITORSEMMAXK,
			VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K,
			VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K,
			VISITORALTMAXK,
			VISITORDIVERSITY,
			VISITORDIVERSITYTC,
			VISITORDIVERSITYTL,
			VISITORDIVERSITYEDM,
			VISITORDIVERSITYEDA,
			VISITORDIVERSITY_ONLY_LAST_EVENT_TC,
			VISITORDIVERSITY_ONLY_LAST_EVENT_TL,
			VISITORDIVERSITY_ONLY_LAST_EVENT_EDA,
			VISITORDIVERSITY_ONLY_LAST_EVENT_EDM,
		};

	/**
	 * A public read-only list of all the '<em><b>Model Visit Strategy Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<ModelVisitStrategyType> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

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
			case VISITORBREADTHFIRST_VALUE: return VISITORBREADTHFIRST;
			case VISITORBREADTHFIRSTWITHGLOBALLOOPS_VALUE: return VISITORBREADTHFIRSTWITHGLOBALLOOPS;
			case VISITORBREADTHFIRSTWITHLOCALLOOPS_VALUE: return VISITORBREADTHFIRSTWITHLOCALLOOPS;
			case VISITORCOVERAGEUNIFORM_VALUE: return VISITORCOVERAGEUNIFORM;
			case VISITORSEQMAXK_VALUE: return VISITORSEQMAXK;
			case VISITORSEQK_VALUE: return VISITORSEQK;
			case VISITORSEMK_VALUE: return VISITORSEMK;
			case VISITORSEMMAXK_VALUE: return VISITORSEMMAXK;
			case VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_VALUE: return VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K;
			case VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_VALUE: return VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K;
			case VISITORALTMAXK_VALUE: return VISITORALTMAXK;
			case VISITORDIVERSITY_VALUE: return VISITORDIVERSITY;
			case VISITORDIVERSITYTC_VALUE: return VISITORDIVERSITYTC;
			case VISITORDIVERSITYTL_VALUE: return VISITORDIVERSITYTL;
			case VISITORDIVERSITYEDM_VALUE: return VISITORDIVERSITYEDM;
			case VISITORDIVERSITYEDA_VALUE: return VISITORDIVERSITYEDA;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_TC_VALUE: return VISITORDIVERSITY_ONLY_LAST_EVENT_TC;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_TL_VALUE: return VISITORDIVERSITY_ONLY_LAST_EVENT_TL;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_VALUE: return VISITORDIVERSITY_ONLY_LAST_EVENT_EDA;
			case VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_VALUE: return VISITORDIVERSITY_ONLY_LAST_EVENT_EDM;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final int value;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String name;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String literal;

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private ModelVisitStrategyType(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getValue() {
	  return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
	  return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLiteral() {
	  return literal;
	}

	/**
	 * Returns the literal value of the enumerator, which is its string representation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}
	
} //ModelVisitStrategyType
