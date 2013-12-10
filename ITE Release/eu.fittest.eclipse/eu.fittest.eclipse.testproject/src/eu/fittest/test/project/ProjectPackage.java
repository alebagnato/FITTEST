/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectFactory
 * @model kind="package"
 * @generated
 */
public interface ProjectPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "project";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.fittest.eu/TestProject";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "project";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	ProjectPackage eINSTANCE = eu.fittest.test.project.impl.ProjectPackageImpl.init();

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.GAParameterTypeImpl <em>GA Parameter Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.GAParameterTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getGAParameterType()
	 * @generated
	 */
	int GA_PARAMETER_TYPE = 0;

	/**
	 * The feature id for the '<em><b>Population Size</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GA_PARAMETER_TYPE__POPULATION_SIZE = 0;

	/**
	 * The feature id for the '<em><b>Max Number Of Generations</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS = 1;

	/**
	 * The feature id for the '<em><b>Mutation Rate</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GA_PARAMETER_TYPE__MUTATION_RATE = 2;

	/**
	 * The feature id for the '<em><b>Time Budget</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GA_PARAMETER_TYPE__TIME_BUDGET = 3;

	/**
	 * The number of structural features of the '<em>GA Parameter Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GA_PARAMETER_TYPE_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>GA Parameter Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GA_PARAMETER_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.GeneralTypeImpl <em>General Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.GeneralTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getGeneralType()
	 * @generated
	 */
	int GENERAL_TYPE = 1;

	/**
	 * The feature id for the '<em><b>Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GENERAL_TYPE__TYPE = 0;

	/**
	 * The feature id for the '<em><b>Base URL</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GENERAL_TYPE__BASE_URL = 1;

	/**
	 * The feature id for the '<em><b>Entry Page</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GENERAL_TYPE__ENTRY_PAGE = 2;

	/**
	 * The feature id for the '<em><b>Server Folder</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GENERAL_TYPE__SERVER_FOLDER = 3;

	/**
	 * The number of structural features of the '<em>General Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GENERAL_TYPE_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>General Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GENERAL_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.InstrumentationTypeImpl <em>Instrumentation Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.InstrumentationTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getInstrumentationType()
	 * @generated
	 */
	int INSTRUMENTATION_TYPE = 2;

	/**
	 * The feature id for the '<em><b>Ghcrt Option</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INSTRUMENTATION_TYPE__GHCRT_OPTION = 0;

	/**
	 * The number of structural features of the '<em>Instrumentation Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INSTRUMENTATION_TYPE_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Instrumentation Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INSTRUMENTATION_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.LoggingTypeImpl <em>Logging Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.LoggingTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getLoggingType()
	 * @generated
	 */
	int LOGGING_TYPE = 3;

	/**
	 * The feature id for the '<em><b>Instrumentation</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGGING_TYPE__INSTRUMENTATION = 0;

	/**
	 * The feature id for the '<em><b>Log Target</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGGING_TYPE__LOG_TARGET = 1;

	/**
	 * The number of structural features of the '<em>Logging Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGGING_TYPE_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Logging Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGGING_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.LogTargetTypeImpl <em>Log Target Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.LogTargetTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getLogTargetType()
	 * @generated
	 */
	int LOG_TARGET_TYPE = 4;

	/**
	 * The feature id for the '<em><b>Store Dir</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOG_TARGET_TYPE__STORE_DIR = 0;

	/**
	 * The feature id for the '<em><b>Log Level</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOG_TARGET_TYPE__LOG_LEVEL = 1;

	/**
	 * The number of structural features of the '<em>Log Target Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOG_TARGET_TYPE_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Log Target Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOG_TARGET_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl <em>Model Inference Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.ModelInferenceTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getModelInferenceType()
	 * @generated
	 */
	int MODEL_INFERENCE_TYPE = 5;

	/**
	 * The feature id for the '<em><b>Domain Input Spec File</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE = 0;

	/**
	 * The feature id for the '<em><b>Abs Func Def File</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE = 1;

	/**
	 * The feature id for the '<em><b>Model File</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE__MODEL_FILE = 2;

	/**
	 * The feature id for the '<em><b>Inference Technique</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE = 3;

	/**
	 * The feature id for the '<em><b>Generate Dot</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE__GENERATE_DOT = 4;

	/**
	 * The feature id for the '<em><b>Ga Param</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE__GA_PARAM = 5;

	/**
	 * The number of structural features of the '<em>Model Inference Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE_FEATURE_COUNT = 6;

	/**
	 * The number of operations of the '<em>Model Inference Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_INFERENCE_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.OracleTypeImpl <em>Oracle Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.OracleTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getOracleType()
	 * @generated
	 */
	int ORACLE_TYPE = 6;

	/**
	 * The number of structural features of the '<em>Oracle Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ORACLE_TYPE_FEATURE_COUNT = 0;

	/**
	 * The number of operations of the '<em>Oracle Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ORACLE_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.TestGenerationTypeImpl <em>Test Generation Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.TestGenerationTypeImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getTestGenerationType()
	 * @generated
	 */
	int TEST_GENERATION_TYPE = 7;

	/**
	 * The feature id for the '<em><b>Cte Folder</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__CTE_FOLDER = 0;

	/**
	 * The feature id for the '<em><b>Model Visit Strategy</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY = 1;

	/**
	 * The feature id for the '<em><b>Reduce Test Suite</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__REDUCE_TEST_SUITE = 2;

	/**
	 * The feature id for the '<em><b>Source Package Prefix</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX = 3;

	/**
	 * The feature id for the '<em><b>Selenium Driver Browser</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER = 4;

	/**
	 * The feature id for the '<em><b>Selenium Remote Host</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST = 5;

	/**
	 * The feature id for the '<em><b>Selenium Remote Port</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT = 6;

	/**
	 * The feature id for the '<em><b>Selenium Browser Config</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG = 7;

	/**
	 * The feature id for the '<em><b>Ga Param</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE__GA_PARAM = 8;

	/**
	 * The number of structural features of the '<em>Test Generation Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE_FEATURE_COUNT = 9;

	/**
	 * The number of operations of the '<em>Test Generation Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_GENERATION_TYPE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.impl.TestProjectImpl <em>Test Project</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.impl.TestProjectImpl
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getTestProject()
	 * @generated
	 */
	int TEST_PROJECT = 8;

	/**
	 * The feature id for the '<em><b>General</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_PROJECT__GENERAL = 0;

	/**
	 * The feature id for the '<em><b>Logging</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_PROJECT__LOGGING = 1;

	/**
	 * The feature id for the '<em><b>Model Inference</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_PROJECT__MODEL_INFERENCE = 2;

	/**
	 * The feature id for the '<em><b>Test Generation</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_PROJECT__TEST_GENERATION = 3;

	/**
	 * The feature id for the '<em><b>Oracle</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_PROJECT__ORACLE = 4;

	/**
	 * The number of structural features of the '<em>Test Project</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_PROJECT_FEATURE_COUNT = 5;

	/**
	 * The number of operations of the '<em>Test Project</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_PROJECT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.BrowserType <em>Browser Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.BrowserType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getBrowserType()
	 * @generated
	 */
	int BROWSER_TYPE = 9;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.InferenceTechniqueType <em>Inference Technique Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.InferenceTechniqueType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getInferenceTechniqueType()
	 * @generated
	 */
	int INFERENCE_TECHNIQUE_TYPE = 10;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.ModelVisitStrategyType <em>Model Visit Strategy Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.ModelVisitStrategyType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getModelVisitStrategyType()
	 * @generated
	 */
	int MODEL_VISIT_STRATEGY_TYPE = 11;

	/**
	 * The meta object id for the '{@link eu.fittest.test.project.SUTTechnologyType <em>SUT Technology Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.SUTTechnologyType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getSUTTechnologyType()
	 * @generated
	 */
	int SUT_TECHNOLOGY_TYPE = 12;

	/**
	 * The meta object id for the '<em>Browser Type Object</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.BrowserType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getBrowserTypeObject()
	 * @generated
	 */
	int BROWSER_TYPE_OBJECT = 13;

	/**
	 * The meta object id for the '<em>Inference Technique Type Object</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.InferenceTechniqueType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getInferenceTechniqueTypeObject()
	 * @generated
	 */
	int INFERENCE_TECHNIQUE_TYPE_OBJECT = 14;

	/**
	 * The meta object id for the '<em>Model Visit Strategy Type Object</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.ModelVisitStrategyType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getModelVisitStrategyTypeObject()
	 * @generated
	 */
	int MODEL_VISIT_STRATEGY_TYPE_OBJECT = 15;

	/**
	 * The meta object id for the '<em>SUT Technology Type Object</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see eu.fittest.test.project.SUTTechnologyType
	 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getSUTTechnologyTypeObject()
	 * @generated
	 */
	int SUT_TECHNOLOGY_TYPE_OBJECT = 16;


	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.GAParameterType <em>GA Parameter Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>GA Parameter Type</em>'.
	 * @see eu.fittest.test.project.GAParameterType
	 * @generated
	 */
	EClass getGAParameterType();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GAParameterType#getPopulationSize <em>Population Size</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Population Size</em>'.
	 * @see eu.fittest.test.project.GAParameterType#getPopulationSize()
	 * @see #getGAParameterType()
	 * @generated
	 */
	EAttribute getGAParameterType_PopulationSize();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GAParameterType#getMaxNumberOfGenerations <em>Max Number Of Generations</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Max Number Of Generations</em>'.
	 * @see eu.fittest.test.project.GAParameterType#getMaxNumberOfGenerations()
	 * @see #getGAParameterType()
	 * @generated
	 */
	EAttribute getGAParameterType_MaxNumberOfGenerations();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GAParameterType#getMutationRate <em>Mutation Rate</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Mutation Rate</em>'.
	 * @see eu.fittest.test.project.GAParameterType#getMutationRate()
	 * @see #getGAParameterType()
	 * @generated
	 */
	EAttribute getGAParameterType_MutationRate();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GAParameterType#getTimeBudget <em>Time Budget</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Time Budget</em>'.
	 * @see eu.fittest.test.project.GAParameterType#getTimeBudget()
	 * @see #getGAParameterType()
	 * @generated
	 */
	EAttribute getGAParameterType_TimeBudget();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.GeneralType <em>General Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>General Type</em>'.
	 * @see eu.fittest.test.project.GeneralType
	 * @generated
	 */
	EClass getGeneralType();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GeneralType#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Type</em>'.
	 * @see eu.fittest.test.project.GeneralType#getType()
	 * @see #getGeneralType()
	 * @generated
	 */
	EAttribute getGeneralType_Type();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GeneralType#getBaseURL <em>Base URL</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Base URL</em>'.
	 * @see eu.fittest.test.project.GeneralType#getBaseURL()
	 * @see #getGeneralType()
	 * @generated
	 */
	EAttribute getGeneralType_BaseURL();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GeneralType#getEntryPage <em>Entry Page</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Entry Page</em>'.
	 * @see eu.fittest.test.project.GeneralType#getEntryPage()
	 * @see #getGeneralType()
	 * @generated
	 */
	EAttribute getGeneralType_EntryPage();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.GeneralType#getServerFolder <em>Server Folder</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Server Folder</em>'.
	 * @see eu.fittest.test.project.GeneralType#getServerFolder()
	 * @see #getGeneralType()
	 * @generated
	 */
	EAttribute getGeneralType_ServerFolder();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.InstrumentationType <em>Instrumentation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Instrumentation Type</em>'.
	 * @see eu.fittest.test.project.InstrumentationType
	 * @generated
	 */
	EClass getInstrumentationType();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.InstrumentationType#getGhcrtOption <em>Ghcrt Option</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ghcrt Option</em>'.
	 * @see eu.fittest.test.project.InstrumentationType#getGhcrtOption()
	 * @see #getInstrumentationType()
	 * @generated
	 */
	EAttribute getInstrumentationType_GhcrtOption();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.LoggingType <em>Logging Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Logging Type</em>'.
	 * @see eu.fittest.test.project.LoggingType
	 * @generated
	 */
	EClass getLoggingType();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.LoggingType#getInstrumentation <em>Instrumentation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Instrumentation</em>'.
	 * @see eu.fittest.test.project.LoggingType#getInstrumentation()
	 * @see #getLoggingType()
	 * @generated
	 */
	EReference getLoggingType_Instrumentation();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.LoggingType#getLogTarget <em>Log Target</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Log Target</em>'.
	 * @see eu.fittest.test.project.LoggingType#getLogTarget()
	 * @see #getLoggingType()
	 * @generated
	 */
	EReference getLoggingType_LogTarget();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.LogTargetType <em>Log Target Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Log Target Type</em>'.
	 * @see eu.fittest.test.project.LogTargetType
	 * @generated
	 */
	EClass getLogTargetType();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.LogTargetType#getStoreDir <em>Store Dir</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Store Dir</em>'.
	 * @see eu.fittest.test.project.LogTargetType#getStoreDir()
	 * @see #getLogTargetType()
	 * @generated
	 */
	EAttribute getLogTargetType_StoreDir();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.LogTargetType#getLogLevel <em>Log Level</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Log Level</em>'.
	 * @see eu.fittest.test.project.LogTargetType#getLogLevel()
	 * @see #getLogTargetType()
	 * @generated
	 */
	EAttribute getLogTargetType_LogLevel();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.ModelInferenceType <em>Model Inference Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Model Inference Type</em>'.
	 * @see eu.fittest.test.project.ModelInferenceType
	 * @generated
	 */
	EClass getModelInferenceType();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.ModelInferenceType#getDomainInputSpecFile <em>Domain Input Spec File</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Domain Input Spec File</em>'.
	 * @see eu.fittest.test.project.ModelInferenceType#getDomainInputSpecFile()
	 * @see #getModelInferenceType()
	 * @generated
	 */
	EAttribute getModelInferenceType_DomainInputSpecFile();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.ModelInferenceType#getAbsFuncDefFile <em>Abs Func Def File</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Abs Func Def File</em>'.
	 * @see eu.fittest.test.project.ModelInferenceType#getAbsFuncDefFile()
	 * @see #getModelInferenceType()
	 * @generated
	 */
	EAttribute getModelInferenceType_AbsFuncDefFile();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.ModelInferenceType#getModelFile <em>Model File</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Model File</em>'.
	 * @see eu.fittest.test.project.ModelInferenceType#getModelFile()
	 * @see #getModelInferenceType()
	 * @generated
	 */
	EAttribute getModelInferenceType_ModelFile();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.ModelInferenceType#getInferenceTechnique <em>Inference Technique</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Inference Technique</em>'.
	 * @see eu.fittest.test.project.ModelInferenceType#getInferenceTechnique()
	 * @see #getModelInferenceType()
	 * @generated
	 */
	EAttribute getModelInferenceType_InferenceTechnique();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.ModelInferenceType#isGenerateDot <em>Generate Dot</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Generate Dot</em>'.
	 * @see eu.fittest.test.project.ModelInferenceType#isGenerateDot()
	 * @see #getModelInferenceType()
	 * @generated
	 */
	EAttribute getModelInferenceType_GenerateDot();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.ModelInferenceType#getGaParam <em>Ga Param</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Ga Param</em>'.
	 * @see eu.fittest.test.project.ModelInferenceType#getGaParam()
	 * @see #getModelInferenceType()
	 * @generated
	 */
	EReference getModelInferenceType_GaParam();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.OracleType <em>Oracle Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Oracle Type</em>'.
	 * @see eu.fittest.test.project.OracleType
	 * @generated
	 */
	EClass getOracleType();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.TestGenerationType <em>Test Generation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Test Generation Type</em>'.
	 * @see eu.fittest.test.project.TestGenerationType
	 * @generated
	 */
	EClass getTestGenerationType();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#getCteFolder <em>Cte Folder</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cte Folder</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getCteFolder()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_CteFolder();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#getModelVisitStrategy <em>Model Visit Strategy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Model Visit Strategy</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getModelVisitStrategy()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_ModelVisitStrategy();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#isReduceTestSuite <em>Reduce Test Suite</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Reduce Test Suite</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#isReduceTestSuite()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_ReduceTestSuite();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#getSourcePackagePrefix <em>Source Package Prefix</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Source Package Prefix</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getSourcePackagePrefix()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_SourcePackagePrefix();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#getSeleniumDriverBrowser <em>Selenium Driver Browser</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Selenium Driver Browser</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getSeleniumDriverBrowser()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_SeleniumDriverBrowser();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemoteHost <em>Selenium Remote Host</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Selenium Remote Host</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getSeleniumRemoteHost()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_SeleniumRemoteHost();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemotePort <em>Selenium Remote Port</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Selenium Remote Port</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getSeleniumRemotePort()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_SeleniumRemotePort();

	/**
	 * Returns the meta object for the attribute '{@link eu.fittest.test.project.TestGenerationType#getSeleniumBrowserConfig <em>Selenium Browser Config</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Selenium Browser Config</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getSeleniumBrowserConfig()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EAttribute getTestGenerationType_SeleniumBrowserConfig();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.TestGenerationType#getGaParam <em>Ga Param</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Ga Param</em>'.
	 * @see eu.fittest.test.project.TestGenerationType#getGaParam()
	 * @see #getTestGenerationType()
	 * @generated
	 */
	EReference getTestGenerationType_GaParam();

	/**
	 * Returns the meta object for class '{@link eu.fittest.test.project.TestProject <em>Test Project</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Test Project</em>'.
	 * @see eu.fittest.test.project.TestProject
	 * @generated
	 */
	EClass getTestProject();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.TestProject#getGeneral <em>General</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>General</em>'.
	 * @see eu.fittest.test.project.TestProject#getGeneral()
	 * @see #getTestProject()
	 * @generated
	 */
	EReference getTestProject_General();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.TestProject#getLogging <em>Logging</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Logging</em>'.
	 * @see eu.fittest.test.project.TestProject#getLogging()
	 * @see #getTestProject()
	 * @generated
	 */
	EReference getTestProject_Logging();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.TestProject#getModelInference <em>Model Inference</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Model Inference</em>'.
	 * @see eu.fittest.test.project.TestProject#getModelInference()
	 * @see #getTestProject()
	 * @generated
	 */
	EReference getTestProject_ModelInference();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.TestProject#getTestGeneration <em>Test Generation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Test Generation</em>'.
	 * @see eu.fittest.test.project.TestProject#getTestGeneration()
	 * @see #getTestProject()
	 * @generated
	 */
	EReference getTestProject_TestGeneration();

	/**
	 * Returns the meta object for the containment reference '{@link eu.fittest.test.project.TestProject#getOracle <em>Oracle</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Oracle</em>'.
	 * @see eu.fittest.test.project.TestProject#getOracle()
	 * @see #getTestProject()
	 * @generated
	 */
	EReference getTestProject_Oracle();

	/**
	 * Returns the meta object for enum '{@link eu.fittest.test.project.BrowserType <em>Browser Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Browser Type</em>'.
	 * @see eu.fittest.test.project.BrowserType
	 * @generated
	 */
	EEnum getBrowserType();

	/**
	 * Returns the meta object for enum '{@link eu.fittest.test.project.InferenceTechniqueType <em>Inference Technique Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Inference Technique Type</em>'.
	 * @see eu.fittest.test.project.InferenceTechniqueType
	 * @generated
	 */
	EEnum getInferenceTechniqueType();

	/**
	 * Returns the meta object for enum '{@link eu.fittest.test.project.ModelVisitStrategyType <em>Model Visit Strategy Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Model Visit Strategy Type</em>'.
	 * @see eu.fittest.test.project.ModelVisitStrategyType
	 * @generated
	 */
	EEnum getModelVisitStrategyType();

	/**
	 * Returns the meta object for enum '{@link eu.fittest.test.project.SUTTechnologyType <em>SUT Technology Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>SUT Technology Type</em>'.
	 * @see eu.fittest.test.project.SUTTechnologyType
	 * @generated
	 */
	EEnum getSUTTechnologyType();

	/**
	 * Returns the meta object for data type '{@link eu.fittest.test.project.BrowserType <em>Browser Type Object</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Browser Type Object</em>'.
	 * @see eu.fittest.test.project.BrowserType
	 * @model instanceClass="eu.fittest.test.project.BrowserType"
	 *        extendedMetaData="name='BrowserType:Object' baseType='BrowserType'"
	 * @generated
	 */
	EDataType getBrowserTypeObject();

	/**
	 * Returns the meta object for data type '{@link eu.fittest.test.project.InferenceTechniqueType <em>Inference Technique Type Object</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Inference Technique Type Object</em>'.
	 * @see eu.fittest.test.project.InferenceTechniqueType
	 * @model instanceClass="eu.fittest.test.project.InferenceTechniqueType"
	 *        extendedMetaData="name='InferenceTechniqueType:Object' baseType='InferenceTechniqueType'"
	 * @generated
	 */
	EDataType getInferenceTechniqueTypeObject();

	/**
	 * Returns the meta object for data type '{@link eu.fittest.test.project.ModelVisitStrategyType <em>Model Visit Strategy Type Object</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Model Visit Strategy Type Object</em>'.
	 * @see eu.fittest.test.project.ModelVisitStrategyType
	 * @model instanceClass="eu.fittest.test.project.ModelVisitStrategyType"
	 *        extendedMetaData="name='ModelVisitStrategyType:Object' baseType='ModelVisitStrategyType'"
	 * @generated
	 */
	EDataType getModelVisitStrategyTypeObject();

	/**
	 * Returns the meta object for data type '{@link eu.fittest.test.project.SUTTechnologyType <em>SUT Technology Type Object</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>SUT Technology Type Object</em>'.
	 * @see eu.fittest.test.project.SUTTechnologyType
	 * @model instanceClass="eu.fittest.test.project.SUTTechnologyType"
	 *        extendedMetaData="name='SUTTechnologyType:Object' baseType='SUTTechnologyType'"
	 * @generated
	 */
	EDataType getSUTTechnologyTypeObject();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	ProjectFactory getProjectFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.GAParameterTypeImpl <em>GA Parameter Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.GAParameterTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getGAParameterType()
		 * @generated
		 */
		EClass GA_PARAMETER_TYPE = eINSTANCE.getGAParameterType();

		/**
		 * The meta object literal for the '<em><b>Population Size</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GA_PARAMETER_TYPE__POPULATION_SIZE = eINSTANCE.getGAParameterType_PopulationSize();

		/**
		 * The meta object literal for the '<em><b>Max Number Of Generations</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS = eINSTANCE.getGAParameterType_MaxNumberOfGenerations();

		/**
		 * The meta object literal for the '<em><b>Mutation Rate</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GA_PARAMETER_TYPE__MUTATION_RATE = eINSTANCE.getGAParameterType_MutationRate();

		/**
		 * The meta object literal for the '<em><b>Time Budget</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GA_PARAMETER_TYPE__TIME_BUDGET = eINSTANCE.getGAParameterType_TimeBudget();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.GeneralTypeImpl <em>General Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.GeneralTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getGeneralType()
		 * @generated
		 */
		EClass GENERAL_TYPE = eINSTANCE.getGeneralType();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GENERAL_TYPE__TYPE = eINSTANCE.getGeneralType_Type();

		/**
		 * The meta object literal for the '<em><b>Base URL</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GENERAL_TYPE__BASE_URL = eINSTANCE.getGeneralType_BaseURL();

		/**
		 * The meta object literal for the '<em><b>Entry Page</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GENERAL_TYPE__ENTRY_PAGE = eINSTANCE.getGeneralType_EntryPage();

		/**
		 * The meta object literal for the '<em><b>Server Folder</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GENERAL_TYPE__SERVER_FOLDER = eINSTANCE.getGeneralType_ServerFolder();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.InstrumentationTypeImpl <em>Instrumentation Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.InstrumentationTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getInstrumentationType()
		 * @generated
		 */
		EClass INSTRUMENTATION_TYPE = eINSTANCE.getInstrumentationType();

		/**
		 * The meta object literal for the '<em><b>Ghcrt Option</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute INSTRUMENTATION_TYPE__GHCRT_OPTION = eINSTANCE.getInstrumentationType_GhcrtOption();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.LoggingTypeImpl <em>Logging Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.LoggingTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getLoggingType()
		 * @generated
		 */
		EClass LOGGING_TYPE = eINSTANCE.getLoggingType();

		/**
		 * The meta object literal for the '<em><b>Instrumentation</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LOGGING_TYPE__INSTRUMENTATION = eINSTANCE.getLoggingType_Instrumentation();

		/**
		 * The meta object literal for the '<em><b>Log Target</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LOGGING_TYPE__LOG_TARGET = eINSTANCE.getLoggingType_LogTarget();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.LogTargetTypeImpl <em>Log Target Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.LogTargetTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getLogTargetType()
		 * @generated
		 */
		EClass LOG_TARGET_TYPE = eINSTANCE.getLogTargetType();

		/**
		 * The meta object literal for the '<em><b>Store Dir</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LOG_TARGET_TYPE__STORE_DIR = eINSTANCE.getLogTargetType_StoreDir();

		/**
		 * The meta object literal for the '<em><b>Log Level</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LOG_TARGET_TYPE__LOG_LEVEL = eINSTANCE.getLogTargetType_LogLevel();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl <em>Model Inference Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.ModelInferenceTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getModelInferenceType()
		 * @generated
		 */
		EClass MODEL_INFERENCE_TYPE = eINSTANCE.getModelInferenceType();

		/**
		 * The meta object literal for the '<em><b>Domain Input Spec File</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE = eINSTANCE.getModelInferenceType_DomainInputSpecFile();

		/**
		 * The meta object literal for the '<em><b>Abs Func Def File</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE = eINSTANCE.getModelInferenceType_AbsFuncDefFile();

		/**
		 * The meta object literal for the '<em><b>Model File</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute MODEL_INFERENCE_TYPE__MODEL_FILE = eINSTANCE.getModelInferenceType_ModelFile();

		/**
		 * The meta object literal for the '<em><b>Inference Technique</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE = eINSTANCE.getModelInferenceType_InferenceTechnique();

		/**
		 * The meta object literal for the '<em><b>Generate Dot</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute MODEL_INFERENCE_TYPE__GENERATE_DOT = eINSTANCE.getModelInferenceType_GenerateDot();

		/**
		 * The meta object literal for the '<em><b>Ga Param</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference MODEL_INFERENCE_TYPE__GA_PARAM = eINSTANCE.getModelInferenceType_GaParam();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.OracleTypeImpl <em>Oracle Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.OracleTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getOracleType()
		 * @generated
		 */
		EClass ORACLE_TYPE = eINSTANCE.getOracleType();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.TestGenerationTypeImpl <em>Test Generation Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.TestGenerationTypeImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getTestGenerationType()
		 * @generated
		 */
		EClass TEST_GENERATION_TYPE = eINSTANCE.getTestGenerationType();

		/**
		 * The meta object literal for the '<em><b>Cte Folder</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__CTE_FOLDER = eINSTANCE.getTestGenerationType_CteFolder();

		/**
		 * The meta object literal for the '<em><b>Model Visit Strategy</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY = eINSTANCE.getTestGenerationType_ModelVisitStrategy();

		/**
		 * The meta object literal for the '<em><b>Reduce Test Suite</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__REDUCE_TEST_SUITE = eINSTANCE.getTestGenerationType_ReduceTestSuite();

		/**
		 * The meta object literal for the '<em><b>Source Package Prefix</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX = eINSTANCE.getTestGenerationType_SourcePackagePrefix();

		/**
		 * The meta object literal for the '<em><b>Selenium Driver Browser</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER = eINSTANCE.getTestGenerationType_SeleniumDriverBrowser();

		/**
		 * The meta object literal for the '<em><b>Selenium Remote Host</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST = eINSTANCE.getTestGenerationType_SeleniumRemoteHost();

		/**
		 * The meta object literal for the '<em><b>Selenium Remote Port</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT = eINSTANCE.getTestGenerationType_SeleniumRemotePort();

		/**
		 * The meta object literal for the '<em><b>Selenium Browser Config</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG = eINSTANCE.getTestGenerationType_SeleniumBrowserConfig();

		/**
		 * The meta object literal for the '<em><b>Ga Param</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TEST_GENERATION_TYPE__GA_PARAM = eINSTANCE.getTestGenerationType_GaParam();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.impl.TestProjectImpl <em>Test Project</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.impl.TestProjectImpl
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getTestProject()
		 * @generated
		 */
		EClass TEST_PROJECT = eINSTANCE.getTestProject();

		/**
		 * The meta object literal for the '<em><b>General</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TEST_PROJECT__GENERAL = eINSTANCE.getTestProject_General();

		/**
		 * The meta object literal for the '<em><b>Logging</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TEST_PROJECT__LOGGING = eINSTANCE.getTestProject_Logging();

		/**
		 * The meta object literal for the '<em><b>Model Inference</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TEST_PROJECT__MODEL_INFERENCE = eINSTANCE.getTestProject_ModelInference();

		/**
		 * The meta object literal for the '<em><b>Test Generation</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TEST_PROJECT__TEST_GENERATION = eINSTANCE.getTestProject_TestGeneration();

		/**
		 * The meta object literal for the '<em><b>Oracle</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TEST_PROJECT__ORACLE = eINSTANCE.getTestProject_Oracle();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.BrowserType <em>Browser Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.BrowserType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getBrowserType()
		 * @generated
		 */
		EEnum BROWSER_TYPE = eINSTANCE.getBrowserType();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.InferenceTechniqueType <em>Inference Technique Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.InferenceTechniqueType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getInferenceTechniqueType()
		 * @generated
		 */
		EEnum INFERENCE_TECHNIQUE_TYPE = eINSTANCE.getInferenceTechniqueType();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.ModelVisitStrategyType <em>Model Visit Strategy Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.ModelVisitStrategyType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getModelVisitStrategyType()
		 * @generated
		 */
		EEnum MODEL_VISIT_STRATEGY_TYPE = eINSTANCE.getModelVisitStrategyType();

		/**
		 * The meta object literal for the '{@link eu.fittest.test.project.SUTTechnologyType <em>SUT Technology Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.SUTTechnologyType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getSUTTechnologyType()
		 * @generated
		 */
		EEnum SUT_TECHNOLOGY_TYPE = eINSTANCE.getSUTTechnologyType();

		/**
		 * The meta object literal for the '<em>Browser Type Object</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.BrowserType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getBrowserTypeObject()
		 * @generated
		 */
		EDataType BROWSER_TYPE_OBJECT = eINSTANCE.getBrowserTypeObject();

		/**
		 * The meta object literal for the '<em>Inference Technique Type Object</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.InferenceTechniqueType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getInferenceTechniqueTypeObject()
		 * @generated
		 */
		EDataType INFERENCE_TECHNIQUE_TYPE_OBJECT = eINSTANCE.getInferenceTechniqueTypeObject();

		/**
		 * The meta object literal for the '<em>Model Visit Strategy Type Object</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.ModelVisitStrategyType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getModelVisitStrategyTypeObject()
		 * @generated
		 */
		EDataType MODEL_VISIT_STRATEGY_TYPE_OBJECT = eINSTANCE.getModelVisitStrategyTypeObject();

		/**
		 * The meta object literal for the '<em>SUT Technology Type Object</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see eu.fittest.test.project.SUTTechnologyType
		 * @see eu.fittest.test.project.impl.ProjectPackageImpl#getSUTTechnologyTypeObject()
		 * @generated
		 */
		EDataType SUT_TECHNOLOGY_TYPE_OBJECT = eINSTANCE.getSUTTechnologyTypeObject();

	}

} //ProjectPackage
