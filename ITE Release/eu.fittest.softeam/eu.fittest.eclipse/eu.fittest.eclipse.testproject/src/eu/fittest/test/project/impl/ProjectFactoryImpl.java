/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class ProjectFactoryImpl extends EFactoryImpl implements ProjectFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static ProjectFactory init() {
		try {
			ProjectFactory theProjectFactory = (ProjectFactory)EPackage.Registry.INSTANCE.getEFactory("http://www.fittest.eu/TestProject"); 
			if (theProjectFactory != null) {
				return theProjectFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new ProjectFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ProjectFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case ProjectPackage.DOCUMENT_ROOT: return createDocumentRoot();
			case ProjectPackage.GA_PARAMETER_TYPE: return createGAParameterType();
			case ProjectPackage.GENERAL_TYPE: return createGeneralType();
			case ProjectPackage.INSTRUMENTATION_TYPE: return createInstrumentationType();
			case ProjectPackage.LOGGING_TYPE: return createLoggingType();
			case ProjectPackage.LOG_TARGET_TYPE: return createLogTargetType();
			case ProjectPackage.MODEL_INFERENCE_TYPE: return createModelInferenceType();
			case ProjectPackage.ORACLE_TYPE: return createOracleType();
			case ProjectPackage.TEST_GENERATION_TYPE: return createTestGenerationType();
			case ProjectPackage.TEST_PROJECT_TYPE: return createTestProjectType();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object createFromString(EDataType eDataType, String initialValue) {
		switch (eDataType.getClassifierID()) {
			case ProjectPackage.BROWSER_TYPE:
				return createBrowserTypeFromString(eDataType, initialValue);
			case ProjectPackage.INFERENCE_TECHNIQUE_TYPE:
				return createInferenceTechniqueTypeFromString(eDataType, initialValue);
			case ProjectPackage.MODEL_VISIT_STRATEGY_TYPE:
				return createModelVisitStrategyTypeFromString(eDataType, initialValue);
			case ProjectPackage.SUT_TECHNOLOGY_TYPE:
				return createSUTTechnologyTypeFromString(eDataType, initialValue);
			case ProjectPackage.BROWSER_TYPE_OBJECT:
				return createBrowserTypeObjectFromString(eDataType, initialValue);
			case ProjectPackage.INFERENCE_TECHNIQUE_TYPE_OBJECT:
				return createInferenceTechniqueTypeObjectFromString(eDataType, initialValue);
			case ProjectPackage.MODEL_VISIT_STRATEGY_TYPE_OBJECT:
				return createModelVisitStrategyTypeObjectFromString(eDataType, initialValue);
			case ProjectPackage.SUT_TECHNOLOGY_TYPE_OBJECT:
				return createSUTTechnologyTypeObjectFromString(eDataType, initialValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertToString(EDataType eDataType, Object instanceValue) {
		switch (eDataType.getClassifierID()) {
			case ProjectPackage.BROWSER_TYPE:
				return convertBrowserTypeToString(eDataType, instanceValue);
			case ProjectPackage.INFERENCE_TECHNIQUE_TYPE:
				return convertInferenceTechniqueTypeToString(eDataType, instanceValue);
			case ProjectPackage.MODEL_VISIT_STRATEGY_TYPE:
				return convertModelVisitStrategyTypeToString(eDataType, instanceValue);
			case ProjectPackage.SUT_TECHNOLOGY_TYPE:
				return convertSUTTechnologyTypeToString(eDataType, instanceValue);
			case ProjectPackage.BROWSER_TYPE_OBJECT:
				return convertBrowserTypeObjectToString(eDataType, instanceValue);
			case ProjectPackage.INFERENCE_TECHNIQUE_TYPE_OBJECT:
				return convertInferenceTechniqueTypeObjectToString(eDataType, instanceValue);
			case ProjectPackage.MODEL_VISIT_STRATEGY_TYPE_OBJECT:
				return convertModelVisitStrategyTypeObjectToString(eDataType, instanceValue);
			case ProjectPackage.SUT_TECHNOLOGY_TYPE_OBJECT:
				return convertSUTTechnologyTypeObjectToString(eDataType, instanceValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DocumentRoot createDocumentRoot() {
		DocumentRootImpl documentRoot = new DocumentRootImpl();
		return documentRoot;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GAParameterType createGAParameterType() {
		GAParameterTypeImpl gaParameterType = new GAParameterTypeImpl();
		return gaParameterType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GeneralType createGeneralType() {
		GeneralTypeImpl generalType = new GeneralTypeImpl();
		return generalType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InstrumentationType createInstrumentationType() {
		InstrumentationTypeImpl instrumentationType = new InstrumentationTypeImpl();
		return instrumentationType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LoggingType createLoggingType() {
		LoggingTypeImpl loggingType = new LoggingTypeImpl();
		return loggingType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LogTargetType createLogTargetType() {
		LogTargetTypeImpl logTargetType = new LogTargetTypeImpl();
		return logTargetType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelInferenceType createModelInferenceType() {
		ModelInferenceTypeImpl modelInferenceType = new ModelInferenceTypeImpl();
		return modelInferenceType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OracleType createOracleType() {
		OracleTypeImpl oracleType = new OracleTypeImpl();
		return oracleType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TestGenerationType createTestGenerationType() {
		TestGenerationTypeImpl testGenerationType = new TestGenerationTypeImpl();
		return testGenerationType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TestProjectType createTestProjectType() {
		TestProjectTypeImpl testProjectType = new TestProjectTypeImpl();
		return testProjectType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BrowserType createBrowserTypeFromString(EDataType eDataType, String initialValue) {
		BrowserType result = BrowserType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertBrowserTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InferenceTechniqueType createInferenceTechniqueTypeFromString(EDataType eDataType, String initialValue) {
		InferenceTechniqueType result = InferenceTechniqueType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertInferenceTechniqueTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelVisitStrategyType createModelVisitStrategyTypeFromString(EDataType eDataType, String initialValue) {
		ModelVisitStrategyType result = ModelVisitStrategyType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertModelVisitStrategyTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SUTTechnologyType createSUTTechnologyTypeFromString(EDataType eDataType, String initialValue) {
		SUTTechnologyType result = SUTTechnologyType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertSUTTechnologyTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BrowserType createBrowserTypeObjectFromString(EDataType eDataType, String initialValue) {
		return createBrowserTypeFromString(ProjectPackage.Literals.BROWSER_TYPE, initialValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertBrowserTypeObjectToString(EDataType eDataType, Object instanceValue) {
		return convertBrowserTypeToString(ProjectPackage.Literals.BROWSER_TYPE, instanceValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InferenceTechniqueType createInferenceTechniqueTypeObjectFromString(EDataType eDataType, String initialValue) {
		return createInferenceTechniqueTypeFromString(ProjectPackage.Literals.INFERENCE_TECHNIQUE_TYPE, initialValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertInferenceTechniqueTypeObjectToString(EDataType eDataType, Object instanceValue) {
		return convertInferenceTechniqueTypeToString(ProjectPackage.Literals.INFERENCE_TECHNIQUE_TYPE, instanceValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelVisitStrategyType createModelVisitStrategyTypeObjectFromString(EDataType eDataType, String initialValue) {
		return createModelVisitStrategyTypeFromString(ProjectPackage.Literals.MODEL_VISIT_STRATEGY_TYPE, initialValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertModelVisitStrategyTypeObjectToString(EDataType eDataType, Object instanceValue) {
		return convertModelVisitStrategyTypeToString(ProjectPackage.Literals.MODEL_VISIT_STRATEGY_TYPE, instanceValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SUTTechnologyType createSUTTechnologyTypeObjectFromString(EDataType eDataType, String initialValue) {
		return createSUTTechnologyTypeFromString(ProjectPackage.Literals.SUT_TECHNOLOGY_TYPE, initialValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertSUTTechnologyTypeObjectToString(EDataType eDataType, Object instanceValue) {
		return convertSUTTechnologyTypeToString(ProjectPackage.Literals.SUT_TECHNOLOGY_TYPE, instanceValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ProjectPackage getProjectPackage() {
		return (ProjectPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	public static ProjectPackage getPackage() {
		return ProjectPackage.eINSTANCE;
	}

} //ProjectFactoryImpl
