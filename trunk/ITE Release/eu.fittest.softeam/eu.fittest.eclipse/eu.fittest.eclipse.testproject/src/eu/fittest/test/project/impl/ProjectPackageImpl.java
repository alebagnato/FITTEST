/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.BrowserType;
import eu.fittest.test.project.DocumentRoot;
import eu.fittest.test.project.GAParameterType;
import eu.fittest.test.project.GeneralType;
import eu.fittest.test.project.InferenceTechniqueType;
import eu.fittest.test.project.InstrumentationType;
import eu.fittest.test.project.LogTargetType;
import eu.fittest.test.project.LoggingType;
import eu.fittest.test.project.ModelInferenceType;
import eu.fittest.test.project.ModelVisitStrategyType;
import eu.fittest.test.project.OracleType;
import eu.fittest.test.project.ProjectFactory;
import eu.fittest.test.project.ProjectPackage;
import eu.fittest.test.project.SUTTechnologyType;
import eu.fittest.test.project.TestGenerationType;
import eu.fittest.test.project.TestProjectType;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.emf.ecore.xml.type.XMLTypePackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class ProjectPackageImpl extends EPackageImpl implements ProjectPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass documentRootEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass gaParameterTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass generalTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass instrumentationTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass loggingTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass logTargetTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass modelInferenceTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass oracleTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass testGenerationTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass testProjectTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum browserTypeEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum inferenceTechniqueTypeEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum modelVisitStrategyTypeEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum sutTechnologyTypeEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType browserTypeObjectEDataType = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType inferenceTechniqueTypeObjectEDataType = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType modelVisitStrategyTypeObjectEDataType = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType sutTechnologyTypeObjectEDataType = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see eu.fittest.test.project.ProjectPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private ProjectPackageImpl() {
		super(eNS_URI, ProjectFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 * 
	 * <p>This method is used to initialize {@link ProjectPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static ProjectPackage init() {
		if (isInited) return (ProjectPackage)EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI);

		// Obtain or create and register package
		ProjectPackageImpl theProjectPackage = (ProjectPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof ProjectPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new ProjectPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		XMLTypePackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theProjectPackage.createPackageContents();

		// Initialize created meta-data
		theProjectPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theProjectPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(ProjectPackage.eNS_URI, theProjectPackage);
		return theProjectPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getDocumentRoot() {
		return documentRootEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getDocumentRoot_Mixed() {
		return (EAttribute)documentRootEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getDocumentRoot_XMLNSPrefixMap() {
		return (EReference)documentRootEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getDocumentRoot_XSISchemaLocation() {
		return (EReference)documentRootEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getDocumentRoot_TestProject() {
		return (EReference)documentRootEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getGAParameterType() {
		return gaParameterTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGAParameterType_PopulationSize() {
		return (EAttribute)gaParameterTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGAParameterType_ChromosomeLength() {
		return (EAttribute)gaParameterTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGAParameterType_MaxNumberOfGenerations() {
		return (EAttribute)gaParameterTypeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGAParameterType_MutationRate() {
		return (EAttribute)gaParameterTypeEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGAParameterType_TimeBudget() {
		return (EAttribute)gaParameterTypeEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGAParameterType_StopPort() {
		return (EAttribute)gaParameterTypeEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getGeneralType() {
		return generalTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGeneralType_Type() {
		return (EAttribute)generalTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGeneralType_BaseURL() {
		return (EAttribute)generalTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGeneralType_EntryPage() {
		return (EAttribute)generalTypeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getGeneralType_ServerFolder() {
		return (EAttribute)generalTypeEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getInstrumentationType() {
		return instrumentationTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getInstrumentationType_GhcrtOption() {
		return (EAttribute)instrumentationTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLoggingType() {
		return loggingTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getLoggingType_Instrumentation() {
		return (EReference)loggingTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getLoggingType_LogTarget() {
		return (EReference)loggingTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLogTargetType() {
		return logTargetTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLogTargetType_StoreDir() {
		return (EAttribute)logTargetTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLogTargetType_LogLevel() {
		return (EAttribute)logTargetTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getModelInferenceType() {
		return modelInferenceTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getModelInferenceType_DomainInputSpecFile() {
		return (EAttribute)modelInferenceTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getModelInferenceType_AbsFuncDefFile() {
		return (EAttribute)modelInferenceTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getModelInferenceType_ModelFile() {
		return (EAttribute)modelInferenceTypeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getModelInferenceType_InferenceTechnique() {
		return (EAttribute)modelInferenceTypeEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getModelInferenceType_GenerateDot() {
		return (EAttribute)modelInferenceTypeEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getModelInferenceType_GaParam() {
		return (EReference)modelInferenceTypeEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getOracleType() {
		return oracleTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_GHCRTopts() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_OracleFile() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_ReportFile() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_EventsToInclude() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_FieldsToInclude() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_FunctionsToInclude() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_LloOption() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getOracleType_ViolationFile() {
		return (EAttribute)oracleTypeEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getTestGenerationType() {
		return testGenerationTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_CteFolder() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_ModelVisitStrategy() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_ReduceTestSuite() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_SourcePackagePrefix() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_SeleniumDriverBrowser() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_SeleniumRemoteHost() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_SeleniumRemotePort() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTestGenerationType_SeleniumBrowserConfig() {
		return (EAttribute)testGenerationTypeEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTestGenerationType_GaParam() {
		return (EReference)testGenerationTypeEClass.getEStructuralFeatures().get(8);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getTestProjectType() {
		return testProjectTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTestProjectType_General() {
		return (EReference)testProjectTypeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTestProjectType_Logging() {
		return (EReference)testProjectTypeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTestProjectType_ModelInference() {
		return (EReference)testProjectTypeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTestProjectType_TestGeneration() {
		return (EReference)testProjectTypeEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTestProjectType_Oracle() {
		return (EReference)testProjectTypeEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getBrowserType() {
		return browserTypeEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getInferenceTechniqueType() {
		return inferenceTechniqueTypeEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getModelVisitStrategyType() {
		return modelVisitStrategyTypeEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getSUTTechnologyType() {
		return sutTechnologyTypeEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EDataType getBrowserTypeObject() {
		return browserTypeObjectEDataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EDataType getInferenceTechniqueTypeObject() {
		return inferenceTechniqueTypeObjectEDataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EDataType getModelVisitStrategyTypeObject() {
		return modelVisitStrategyTypeObjectEDataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EDataType getSUTTechnologyTypeObject() {
		return sutTechnologyTypeObjectEDataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ProjectFactory getProjectFactory() {
		return (ProjectFactory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		documentRootEClass = createEClass(DOCUMENT_ROOT);
		createEAttribute(documentRootEClass, DOCUMENT_ROOT__MIXED);
		createEReference(documentRootEClass, DOCUMENT_ROOT__XMLNS_PREFIX_MAP);
		createEReference(documentRootEClass, DOCUMENT_ROOT__XSI_SCHEMA_LOCATION);
		createEReference(documentRootEClass, DOCUMENT_ROOT__TEST_PROJECT);

		gaParameterTypeEClass = createEClass(GA_PARAMETER_TYPE);
		createEAttribute(gaParameterTypeEClass, GA_PARAMETER_TYPE__POPULATION_SIZE);
		createEAttribute(gaParameterTypeEClass, GA_PARAMETER_TYPE__CHROMOSOME_LENGTH);
		createEAttribute(gaParameterTypeEClass, GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS);
		createEAttribute(gaParameterTypeEClass, GA_PARAMETER_TYPE__MUTATION_RATE);
		createEAttribute(gaParameterTypeEClass, GA_PARAMETER_TYPE__TIME_BUDGET);
		createEAttribute(gaParameterTypeEClass, GA_PARAMETER_TYPE__STOP_PORT);

		generalTypeEClass = createEClass(GENERAL_TYPE);
		createEAttribute(generalTypeEClass, GENERAL_TYPE__TYPE);
		createEAttribute(generalTypeEClass, GENERAL_TYPE__BASE_URL);
		createEAttribute(generalTypeEClass, GENERAL_TYPE__ENTRY_PAGE);
		createEAttribute(generalTypeEClass, GENERAL_TYPE__SERVER_FOLDER);

		instrumentationTypeEClass = createEClass(INSTRUMENTATION_TYPE);
		createEAttribute(instrumentationTypeEClass, INSTRUMENTATION_TYPE__GHCRT_OPTION);

		loggingTypeEClass = createEClass(LOGGING_TYPE);
		createEReference(loggingTypeEClass, LOGGING_TYPE__INSTRUMENTATION);
		createEReference(loggingTypeEClass, LOGGING_TYPE__LOG_TARGET);

		logTargetTypeEClass = createEClass(LOG_TARGET_TYPE);
		createEAttribute(logTargetTypeEClass, LOG_TARGET_TYPE__STORE_DIR);
		createEAttribute(logTargetTypeEClass, LOG_TARGET_TYPE__LOG_LEVEL);

		modelInferenceTypeEClass = createEClass(MODEL_INFERENCE_TYPE);
		createEAttribute(modelInferenceTypeEClass, MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE);
		createEAttribute(modelInferenceTypeEClass, MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE);
		createEAttribute(modelInferenceTypeEClass, MODEL_INFERENCE_TYPE__MODEL_FILE);
		createEAttribute(modelInferenceTypeEClass, MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE);
		createEAttribute(modelInferenceTypeEClass, MODEL_INFERENCE_TYPE__GENERATE_DOT);
		createEReference(modelInferenceTypeEClass, MODEL_INFERENCE_TYPE__GA_PARAM);

		oracleTypeEClass = createEClass(ORACLE_TYPE);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__GHCR_TOPTS);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__ORACLE_FILE);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__REPORT_FILE);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__EVENTS_TO_INCLUDE);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__FIELDS_TO_INCLUDE);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__FUNCTIONS_TO_INCLUDE);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__LLO_OPTION);
		createEAttribute(oracleTypeEClass, ORACLE_TYPE__VIOLATION_FILE);

		testGenerationTypeEClass = createEClass(TEST_GENERATION_TYPE);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__CTE_FOLDER);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__REDUCE_TEST_SUITE);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT);
		createEAttribute(testGenerationTypeEClass, TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG);
		createEReference(testGenerationTypeEClass, TEST_GENERATION_TYPE__GA_PARAM);

		testProjectTypeEClass = createEClass(TEST_PROJECT_TYPE);
		createEReference(testProjectTypeEClass, TEST_PROJECT_TYPE__GENERAL);
		createEReference(testProjectTypeEClass, TEST_PROJECT_TYPE__LOGGING);
		createEReference(testProjectTypeEClass, TEST_PROJECT_TYPE__MODEL_INFERENCE);
		createEReference(testProjectTypeEClass, TEST_PROJECT_TYPE__TEST_GENERATION);
		createEReference(testProjectTypeEClass, TEST_PROJECT_TYPE__ORACLE);

		// Create enums
		browserTypeEEnum = createEEnum(BROWSER_TYPE);
		inferenceTechniqueTypeEEnum = createEEnum(INFERENCE_TECHNIQUE_TYPE);
		modelVisitStrategyTypeEEnum = createEEnum(MODEL_VISIT_STRATEGY_TYPE);
		sutTechnologyTypeEEnum = createEEnum(SUT_TECHNOLOGY_TYPE);

		// Create data types
		browserTypeObjectEDataType = createEDataType(BROWSER_TYPE_OBJECT);
		inferenceTechniqueTypeObjectEDataType = createEDataType(INFERENCE_TECHNIQUE_TYPE_OBJECT);
		modelVisitStrategyTypeObjectEDataType = createEDataType(MODEL_VISIT_STRATEGY_TYPE_OBJECT);
		sutTechnologyTypeObjectEDataType = createEDataType(SUT_TECHNOLOGY_TYPE_OBJECT);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		XMLTypePackage theXMLTypePackage = (XMLTypePackage)EPackage.Registry.INSTANCE.getEPackage(XMLTypePackage.eNS_URI);

		// Add supertypes to classes

		// Initialize classes and features; add operations and parameters
		initEClass(documentRootEClass, DocumentRoot.class, "DocumentRoot", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDocumentRoot_Mixed(), ecorePackage.getEFeatureMapEntry(), "mixed", null, 0, -1, null, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDocumentRoot_XMLNSPrefixMap(), ecorePackage.getEStringToStringMapEntry(), null, "xMLNSPrefixMap", null, 0, -1, null, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDocumentRoot_XSISchemaLocation(), ecorePackage.getEStringToStringMapEntry(), null, "xSISchemaLocation", null, 0, -1, null, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDocumentRoot_TestProject(), this.getTestProjectType(), null, "testProject", null, 0, -2, null, IS_TRANSIENT, IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, IS_DERIVED, IS_ORDERED);

		initEClass(gaParameterTypeEClass, GAParameterType.class, "GAParameterType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getGAParameterType_PopulationSize(), theXMLTypePackage.getInt(), "populationSize", null, 1, 1, GAParameterType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGAParameterType_ChromosomeLength(), theXMLTypePackage.getInt(), "chromosomeLength", null, 1, 1, GAParameterType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGAParameterType_MaxNumberOfGenerations(), theXMLTypePackage.getInt(), "maxNumberOfGenerations", null, 1, 1, GAParameterType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGAParameterType_MutationRate(), theXMLTypePackage.getDouble(), "mutationRate", null, 1, 1, GAParameterType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGAParameterType_TimeBudget(), theXMLTypePackage.getInt(), "timeBudget", null, 1, 1, GAParameterType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGAParameterType_StopPort(), theXMLTypePackage.getInt(), "stopPort", null, 1, 1, GAParameterType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(generalTypeEClass, GeneralType.class, "GeneralType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getGeneralType_Type(), this.getSUTTechnologyType(), "type", "FLASH", 1, 1, GeneralType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGeneralType_BaseURL(), theXMLTypePackage.getString(), "baseURL", null, 1, 1, GeneralType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGeneralType_EntryPage(), theXMLTypePackage.getString(), "entryPage", null, 1, 1, GeneralType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGeneralType_ServerFolder(), theXMLTypePackage.getString(), "serverFolder", null, 0, 1, GeneralType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(instrumentationTypeEClass, InstrumentationType.class, "InstrumentationType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getInstrumentationType_GhcrtOption(), theXMLTypePackage.getString(), "ghcrtOption", null, 1, 1, InstrumentationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(loggingTypeEClass, LoggingType.class, "LoggingType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getLoggingType_Instrumentation(), this.getInstrumentationType(), null, "instrumentation", null, 1, 1, LoggingType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getLoggingType_LogTarget(), this.getLogTargetType(), null, "logTarget", null, 1, 1, LoggingType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(logTargetTypeEClass, LogTargetType.class, "LogTargetType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getLogTargetType_StoreDir(), theXMLTypePackage.getString(), "storeDir", null, 1, 1, LogTargetType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLogTargetType_LogLevel(), theXMLTypePackage.getInt(), "logLevel", null, 1, 1, LogTargetType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(modelInferenceTypeEClass, ModelInferenceType.class, "ModelInferenceType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getModelInferenceType_DomainInputSpecFile(), theXMLTypePackage.getString(), "domainInputSpecFile", null, 1, 1, ModelInferenceType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getModelInferenceType_AbsFuncDefFile(), theXMLTypePackage.getString(), "absFuncDefFile", null, 1, 1, ModelInferenceType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getModelInferenceType_ModelFile(), theXMLTypePackage.getString(), "modelFile", null, 1, 1, ModelInferenceType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getModelInferenceType_InferenceTechnique(), this.getInferenceTechniqueType(), "inferenceTechnique", null, 1, 1, ModelInferenceType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getModelInferenceType_GenerateDot(), theXMLTypePackage.getBoolean(), "generateDot", null, 1, 1, ModelInferenceType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getModelInferenceType_GaParam(), this.getGAParameterType(), null, "gaParam", null, 1, 1, ModelInferenceType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(oracleTypeEClass, OracleType.class, "OracleType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getOracleType_GHCRTopts(), theXMLTypePackage.getString(), "gHCRTopts", null, 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOracleType_OracleFile(), theXMLTypePackage.getString(), "oracleFile", "oracle.inv", 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOracleType_ReportFile(), theXMLTypePackage.getString(), "reportFile", "report.txt", 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOracleType_EventsToInclude(), theXMLTypePackage.getString(), "eventsToInclude", null, 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOracleType_FieldsToInclude(), theXMLTypePackage.getString(), "fieldsToInclude", null, 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOracleType_FunctionsToInclude(), theXMLTypePackage.getString(), "functionsToInclude", null, 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOracleType_LloOption(), theXMLTypePackage.getString(), "lloOption", null, 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOracleType_ViolationFile(), theXMLTypePackage.getString(), "violationFile", "violations.txt", 1, 1, OracleType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(testGenerationTypeEClass, TestGenerationType.class, "TestGenerationType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTestGenerationType_CteFolder(), theXMLTypePackage.getString(), "cteFolder", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTestGenerationType_ModelVisitStrategy(), this.getModelVisitStrategyType(), "modelVisitStrategy", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTestGenerationType_ReduceTestSuite(), theXMLTypePackage.getBoolean(), "reduceTestSuite", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTestGenerationType_SourcePackagePrefix(), theXMLTypePackage.getString(), "sourcePackagePrefix", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTestGenerationType_SeleniumDriverBrowser(), this.getBrowserType(), "seleniumDriverBrowser", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTestGenerationType_SeleniumRemoteHost(), theXMLTypePackage.getString(), "seleniumRemoteHost", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTestGenerationType_SeleniumRemotePort(), theXMLTypePackage.getInt(), "seleniumRemotePort", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTestGenerationType_SeleniumBrowserConfig(), theXMLTypePackage.getString(), "seleniumBrowserConfig", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTestGenerationType_GaParam(), this.getGAParameterType(), null, "gaParam", null, 1, 1, TestGenerationType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(testProjectTypeEClass, TestProjectType.class, "TestProjectType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getTestProjectType_General(), this.getGeneralType(), null, "general", null, 1, 1, TestProjectType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTestProjectType_Logging(), this.getLoggingType(), null, "logging", null, 1, 1, TestProjectType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTestProjectType_ModelInference(), this.getModelInferenceType(), null, "modelInference", null, 1, 1, TestProjectType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTestProjectType_TestGeneration(), this.getTestGenerationType(), null, "testGeneration", null, 1, 1, TestProjectType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTestProjectType_Oracle(), this.getOracleType(), null, "oracle", null, 1, 1, TestProjectType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(browserTypeEEnum, BrowserType.class, "BrowserType");
		addEEnumLiteral(browserTypeEEnum, BrowserType.HTML_UNIT_DRIVER_LITERAL);
		addEEnumLiteral(browserTypeEEnum, BrowserType.FIREFOX_DRIVER_LITERAL);
		addEEnumLiteral(browserTypeEEnum, BrowserType.CHROME_DRIVER_LITERAL);
		addEEnumLiteral(browserTypeEEnum, BrowserType.INTERNET_EXPLORER_DRIVER_LITERAL);
		addEEnumLiteral(browserTypeEEnum, BrowserType.FLEX_OBJECT_DRIVER_LITERAL);
		addEEnumLiteral(browserTypeEEnum, BrowserType.FLASH_APPLICATION_LITERAL);

		initEEnum(inferenceTechniqueTypeEEnum, InferenceTechniqueType.class, "InferenceTechniqueType");
		addEEnumLiteral(inferenceTechniqueTypeEEnum, InferenceTechniqueType.SEQUENCEBASED_LITERAL);
		addEEnumLiteral(inferenceTechniqueTypeEEnum, InferenceTechniqueType.STATEBASED_LITERAL);
		addEEnumLiteral(inferenceTechniqueTypeEEnum, InferenceTechniqueType.AUTOABS_LITERAL);

		initEEnum(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.class, "ModelVisitStrategyType");
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORBREADTHFIRST_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORBREADTHFIRSTWITHGLOBALLOOPS_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORBREADTHFIRSTWITHLOCALLOOPS_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORCOVERAGEUNIFORM_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORSEQMAXK_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORSEQK_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORSEMK_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORSEMMAXK_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORSE_MSEXTRACTOR_ONLY_LAST_EVENT_K_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORALTMAXK_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITY_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITYTC_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITYTL_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITYEDM_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITYEDA_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITY_ONLY_LAST_EVENT_TC_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITY_ONLY_LAST_EVENT_TL_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITY_ONLY_LAST_EVENT_EDA_LITERAL);
		addEEnumLiteral(modelVisitStrategyTypeEEnum, ModelVisitStrategyType.VISITORDIVERSITY_ONLY_LAST_EVENT_EDM_LITERAL);

		initEEnum(sutTechnologyTypeEEnum, SUTTechnologyType.class, "SUTTechnologyType");
		addEEnumLiteral(sutTechnologyTypeEEnum, SUTTechnologyType.FLASH_LITERAL);
		addEEnumLiteral(sutTechnologyTypeEEnum, SUTTechnologyType.PHP_LITERAL);
		addEEnumLiteral(sutTechnologyTypeEEnum, SUTTechnologyType.HTML_LITERAL);

		// Initialize data types
		initEDataType(browserTypeObjectEDataType, BrowserType.class, "BrowserTypeObject", IS_SERIALIZABLE, IS_GENERATED_INSTANCE_CLASS);
		initEDataType(inferenceTechniqueTypeObjectEDataType, InferenceTechniqueType.class, "InferenceTechniqueTypeObject", IS_SERIALIZABLE, IS_GENERATED_INSTANCE_CLASS);
		initEDataType(modelVisitStrategyTypeObjectEDataType, ModelVisitStrategyType.class, "ModelVisitStrategyTypeObject", IS_SERIALIZABLE, IS_GENERATED_INSTANCE_CLASS);
		initEDataType(sutTechnologyTypeObjectEDataType, SUTTechnologyType.class, "SUTTechnologyTypeObject", IS_SERIALIZABLE, IS_GENERATED_INSTANCE_CLASS);

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// http:///org/eclipse/emf/ecore/util/ExtendedMetaData
		createExtendedMetaDataAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http:///org/eclipse/emf/ecore/util/ExtendedMetaData</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createExtendedMetaDataAnnotations() {
		String source = "http:///org/eclipse/emf/ecore/util/ExtendedMetaData";		
		addAnnotation
		  (browserTypeEEnum, 
		   source, 
		   new String[] {
			 "name", "BrowserType"
		   });		
		addAnnotation
		  (browserTypeObjectEDataType, 
		   source, 
		   new String[] {
			 "name", "BrowserType:Object",
			 "baseType", "BrowserType"
		   });		
		addAnnotation
		  (documentRootEClass, 
		   source, 
		   new String[] {
			 "name", "",
			 "kind", "mixed"
		   });		
		addAnnotation
		  (getDocumentRoot_Mixed(), 
		   source, 
		   new String[] {
			 "kind", "elementWildcard",
			 "name", ":mixed"
		   });		
		addAnnotation
		  (getDocumentRoot_XMLNSPrefixMap(), 
		   source, 
		   new String[] {
			 "kind", "attribute",
			 "name", "xmlns:prefix"
		   });		
		addAnnotation
		  (getDocumentRoot_XSISchemaLocation(), 
		   source, 
		   new String[] {
			 "kind", "attribute",
			 "name", "xsi:schemaLocation"
		   });		
		addAnnotation
		  (getDocumentRoot_TestProject(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "TestProject",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (gaParameterTypeEClass, 
		   source, 
		   new String[] {
			 "name", "GAParameterType",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getGAParameterType_PopulationSize(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "populationSize",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getGAParameterType_ChromosomeLength(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "chromosomeLength",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getGAParameterType_MaxNumberOfGenerations(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "maxNumberOfGenerations",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getGAParameterType_MutationRate(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "mutationRate",
			 "namespace", "##targetNamespace"
		   });			
		addAnnotation
		  (getGAParameterType_TimeBudget(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "timeBudget",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getGAParameterType_StopPort(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "stopPort",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (generalTypeEClass, 
		   source, 
		   new String[] {
			 "name", "GeneralType",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getGeneralType_Type(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "type",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getGeneralType_BaseURL(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "baseURL",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getGeneralType_EntryPage(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "entryPage",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getGeneralType_ServerFolder(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "serverFolder",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (inferenceTechniqueTypeEEnum, 
		   source, 
		   new String[] {
			 "name", "InferenceTechniqueType"
		   });		
		addAnnotation
		  (inferenceTechniqueTypeObjectEDataType, 
		   source, 
		   new String[] {
			 "name", "InferenceTechniqueType:Object",
			 "baseType", "InferenceTechniqueType"
		   });		
		addAnnotation
		  (instrumentationTypeEClass, 
		   source, 
		   new String[] {
			 "name", "InstrumentationType",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getInstrumentationType_GhcrtOption(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "ghcrtOption",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (loggingTypeEClass, 
		   source, 
		   new String[] {
			 "name", "LoggingType",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getLoggingType_Instrumentation(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "Instrumentation",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getLoggingType_LogTarget(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "LogTarget",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (logTargetTypeEClass, 
		   source, 
		   new String[] {
			 "name", "LogTargetType",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getLogTargetType_StoreDir(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "storeDir",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getLogTargetType_LogLevel(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "logLevel",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (modelInferenceTypeEClass, 
		   source, 
		   new String[] {
			 "name", "ModelInferenceType",
			 "kind", "elementOnly"
		   });			
		addAnnotation
		  (getModelInferenceType_DomainInputSpecFile(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "domainInputSpecFile",
			 "namespace", "##targetNamespace"
		   });			
		addAnnotation
		  (getModelInferenceType_AbsFuncDefFile(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "absFuncDefFile",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getModelInferenceType_ModelFile(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "modelFile",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getModelInferenceType_InferenceTechnique(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "inferenceTechnique",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getModelInferenceType_GenerateDot(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "generateDot",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getModelInferenceType_GaParam(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "gaParam",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (modelVisitStrategyTypeEEnum, 
		   source, 
		   new String[] {
			 "name", "ModelVisitStrategyType"
		   });		
		addAnnotation
		  (modelVisitStrategyTypeObjectEDataType, 
		   source, 
		   new String[] {
			 "name", "ModelVisitStrategyType:Object",
			 "baseType", "ModelVisitStrategyType"
		   });		
		addAnnotation
		  (oracleTypeEClass, 
		   source, 
		   new String[] {
			 "name", "OracleType",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getOracleType_GHCRTopts(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "GHCRTopts",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getOracleType_OracleFile(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "oracleFile",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getOracleType_ReportFile(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "reportFile",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getOracleType_EventsToInclude(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "eventsToInclude",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getOracleType_FieldsToInclude(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "fieldsToInclude",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getOracleType_FunctionsToInclude(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "functionsToInclude",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getOracleType_LloOption(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "lloOption",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getOracleType_ViolationFile(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "violationFile",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (sutTechnologyTypeEEnum, 
		   source, 
		   new String[] {
			 "name", "SUTTechnologyType"
		   });		
		addAnnotation
		  (sutTechnologyTypeObjectEDataType, 
		   source, 
		   new String[] {
			 "name", "SUTTechnologyType:Object",
			 "baseType", "SUTTechnologyType"
		   });		
		addAnnotation
		  (testGenerationTypeEClass, 
		   source, 
		   new String[] {
			 "name", "TestGenerationType",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getTestGenerationType_CteFolder(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "cteFolder",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_ModelVisitStrategy(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "modelVisitStrategy",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_ReduceTestSuite(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "reduceTestSuite",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_SourcePackagePrefix(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "sourcePackagePrefix",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_SeleniumDriverBrowser(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "seleniumDriverBrowser",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_SeleniumRemoteHost(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "seleniumRemoteHost",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_SeleniumRemotePort(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "seleniumRemotePort",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_SeleniumBrowserConfig(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "seleniumBrowserConfig",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestGenerationType_GaParam(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "gaParam",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (testProjectTypeEClass, 
		   source, 
		   new String[] {
			 "name", "TestProject_._type",
			 "kind", "elementOnly"
		   });		
		addAnnotation
		  (getTestProjectType_General(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "General",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestProjectType_Logging(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "Logging",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestProjectType_ModelInference(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "ModelInference",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestProjectType_TestGeneration(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "TestGeneration",
			 "namespace", "##targetNamespace"
		   });		
		addAnnotation
		  (getTestProjectType_Oracle(), 
		   source, 
		   new String[] {
			 "kind", "element",
			 "name", "Oracle",
			 "namespace", "##targetNamespace"
		   });
	}

} //ProjectPackageImpl
