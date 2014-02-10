/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.BrowserType;
import eu.fittest.test.project.GAParameterType;
import eu.fittest.test.project.ModelVisitStrategyType;
import eu.fittest.test.project.ProjectPackage;
import eu.fittest.test.project.TestGenerationType;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Test Generation Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getCteFolder <em>Cte Folder</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getModelVisitStrategy <em>Model Visit Strategy</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#isReduceTestSuite <em>Reduce Test Suite</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getSourcePackagePrefix <em>Source Package Prefix</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getSeleniumDriverBrowser <em>Selenium Driver Browser</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getSeleniumRemoteHost <em>Selenium Remote Host</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getSeleniumRemotePort <em>Selenium Remote Port</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getSeleniumBrowserConfig <em>Selenium Browser Config</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestGenerationTypeImpl#getGaParam <em>Ga Param</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TestGenerationTypeImpl extends EObjectImpl implements TestGenerationType {
	/**
	 * The default value of the '{@link #getCteFolder() <em>Cte Folder</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCteFolder()
	 * @generated
	 * @ordered
	 */
	protected static final String CTE_FOLDER_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getCteFolder() <em>Cte Folder</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCteFolder()
	 * @generated
	 * @ordered
	 */
	protected String cteFolder = CTE_FOLDER_EDEFAULT;

	/**
	 * The default value of the '{@link #getModelVisitStrategy() <em>Model Visit Strategy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelVisitStrategy()
	 * @generated
	 * @ordered
	 */
	protected static final ModelVisitStrategyType MODEL_VISIT_STRATEGY_EDEFAULT = ModelVisitStrategyType.VISITORBREADTHFIRST_LITERAL;

	/**
	 * The cached value of the '{@link #getModelVisitStrategy() <em>Model Visit Strategy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelVisitStrategy()
	 * @generated
	 * @ordered
	 */
	protected ModelVisitStrategyType modelVisitStrategy = MODEL_VISIT_STRATEGY_EDEFAULT;

	/**
	 * This is true if the Model Visit Strategy attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean modelVisitStrategyESet;

	/**
	 * The default value of the '{@link #isReduceTestSuite() <em>Reduce Test Suite</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isReduceTestSuite()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REDUCE_TEST_SUITE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isReduceTestSuite() <em>Reduce Test Suite</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isReduceTestSuite()
	 * @generated
	 * @ordered
	 */
	protected boolean reduceTestSuite = REDUCE_TEST_SUITE_EDEFAULT;

	/**
	 * This is true if the Reduce Test Suite attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean reduceTestSuiteESet;

	/**
	 * The default value of the '{@link #getSourcePackagePrefix() <em>Source Package Prefix</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSourcePackagePrefix()
	 * @generated
	 * @ordered
	 */
	protected static final String SOURCE_PACKAGE_PREFIX_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSourcePackagePrefix() <em>Source Package Prefix</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSourcePackagePrefix()
	 * @generated
	 * @ordered
	 */
	protected String sourcePackagePrefix = SOURCE_PACKAGE_PREFIX_EDEFAULT;

	/**
	 * The default value of the '{@link #getSeleniumDriverBrowser() <em>Selenium Driver Browser</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumDriverBrowser()
	 * @generated
	 * @ordered
	 */
	protected static final BrowserType SELENIUM_DRIVER_BROWSER_EDEFAULT = BrowserType.HTML_UNIT_DRIVER_LITERAL;

	/**
	 * The cached value of the '{@link #getSeleniumDriverBrowser() <em>Selenium Driver Browser</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumDriverBrowser()
	 * @generated
	 * @ordered
	 */
	protected BrowserType seleniumDriverBrowser = SELENIUM_DRIVER_BROWSER_EDEFAULT;

	/**
	 * This is true if the Selenium Driver Browser attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean seleniumDriverBrowserESet;

	/**
	 * The default value of the '{@link #getSeleniumRemoteHost() <em>Selenium Remote Host</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumRemoteHost()
	 * @generated
	 * @ordered
	 */
	protected static final String SELENIUM_REMOTE_HOST_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSeleniumRemoteHost() <em>Selenium Remote Host</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumRemoteHost()
	 * @generated
	 * @ordered
	 */
	protected String seleniumRemoteHost = SELENIUM_REMOTE_HOST_EDEFAULT;

	/**
	 * The default value of the '{@link #getSeleniumRemotePort() <em>Selenium Remote Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumRemotePort()
	 * @generated
	 * @ordered
	 */
	protected static final int SELENIUM_REMOTE_PORT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getSeleniumRemotePort() <em>Selenium Remote Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumRemotePort()
	 * @generated
	 * @ordered
	 */
	protected int seleniumRemotePort = SELENIUM_REMOTE_PORT_EDEFAULT;

	/**
	 * This is true if the Selenium Remote Port attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean seleniumRemotePortESet;

	/**
	 * The default value of the '{@link #getSeleniumBrowserConfig() <em>Selenium Browser Config</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumBrowserConfig()
	 * @generated
	 * @ordered
	 */
	protected static final String SELENIUM_BROWSER_CONFIG_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSeleniumBrowserConfig() <em>Selenium Browser Config</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeleniumBrowserConfig()
	 * @generated
	 * @ordered
	 */
	protected String seleniumBrowserConfig = SELENIUM_BROWSER_CONFIG_EDEFAULT;

	/**
	 * The cached value of the '{@link #getGaParam() <em>Ga Param</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGaParam()
	 * @generated
	 * @ordered
	 */
	protected GAParameterType gaParam;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TestGenerationTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.TEST_GENERATION_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getCteFolder() {
		return cteFolder;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setCteFolder(String newCteFolder) {
		String oldCteFolder = cteFolder;
		cteFolder = newCteFolder;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__CTE_FOLDER, oldCteFolder, cteFolder));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelVisitStrategyType getModelVisitStrategy() {
		return modelVisitStrategy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setModelVisitStrategy(ModelVisitStrategyType newModelVisitStrategy) {
		ModelVisitStrategyType oldModelVisitStrategy = modelVisitStrategy;
		modelVisitStrategy = newModelVisitStrategy == null ? MODEL_VISIT_STRATEGY_EDEFAULT : newModelVisitStrategy;
		boolean oldModelVisitStrategyESet = modelVisitStrategyESet;
		modelVisitStrategyESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY, oldModelVisitStrategy, modelVisitStrategy, !oldModelVisitStrategyESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetModelVisitStrategy() {
		ModelVisitStrategyType oldModelVisitStrategy = modelVisitStrategy;
		boolean oldModelVisitStrategyESet = modelVisitStrategyESet;
		modelVisitStrategy = MODEL_VISIT_STRATEGY_EDEFAULT;
		modelVisitStrategyESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY, oldModelVisitStrategy, MODEL_VISIT_STRATEGY_EDEFAULT, oldModelVisitStrategyESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetModelVisitStrategy() {
		return modelVisitStrategyESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isReduceTestSuite() {
		return reduceTestSuite;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setReduceTestSuite(boolean newReduceTestSuite) {
		boolean oldReduceTestSuite = reduceTestSuite;
		reduceTestSuite = newReduceTestSuite;
		boolean oldReduceTestSuiteESet = reduceTestSuiteESet;
		reduceTestSuiteESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE, oldReduceTestSuite, reduceTestSuite, !oldReduceTestSuiteESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetReduceTestSuite() {
		boolean oldReduceTestSuite = reduceTestSuite;
		boolean oldReduceTestSuiteESet = reduceTestSuiteESet;
		reduceTestSuite = REDUCE_TEST_SUITE_EDEFAULT;
		reduceTestSuiteESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE, oldReduceTestSuite, REDUCE_TEST_SUITE_EDEFAULT, oldReduceTestSuiteESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetReduceTestSuite() {
		return reduceTestSuiteESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getSourcePackagePrefix() {
		return sourcePackagePrefix;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSourcePackagePrefix(String newSourcePackagePrefix) {
		String oldSourcePackagePrefix = sourcePackagePrefix;
		sourcePackagePrefix = newSourcePackagePrefix;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX, oldSourcePackagePrefix, sourcePackagePrefix));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BrowserType getSeleniumDriverBrowser() {
		return seleniumDriverBrowser;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSeleniumDriverBrowser(BrowserType newSeleniumDriverBrowser) {
		BrowserType oldSeleniumDriverBrowser = seleniumDriverBrowser;
		seleniumDriverBrowser = newSeleniumDriverBrowser == null ? SELENIUM_DRIVER_BROWSER_EDEFAULT : newSeleniumDriverBrowser;
		boolean oldSeleniumDriverBrowserESet = seleniumDriverBrowserESet;
		seleniumDriverBrowserESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER, oldSeleniumDriverBrowser, seleniumDriverBrowser, !oldSeleniumDriverBrowserESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetSeleniumDriverBrowser() {
		BrowserType oldSeleniumDriverBrowser = seleniumDriverBrowser;
		boolean oldSeleniumDriverBrowserESet = seleniumDriverBrowserESet;
		seleniumDriverBrowser = SELENIUM_DRIVER_BROWSER_EDEFAULT;
		seleniumDriverBrowserESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER, oldSeleniumDriverBrowser, SELENIUM_DRIVER_BROWSER_EDEFAULT, oldSeleniumDriverBrowserESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetSeleniumDriverBrowser() {
		return seleniumDriverBrowserESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getSeleniumRemoteHost() {
		return seleniumRemoteHost;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSeleniumRemoteHost(String newSeleniumRemoteHost) {
		String oldSeleniumRemoteHost = seleniumRemoteHost;
		seleniumRemoteHost = newSeleniumRemoteHost;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST, oldSeleniumRemoteHost, seleniumRemoteHost));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getSeleniumRemotePort() {
		return seleniumRemotePort;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSeleniumRemotePort(int newSeleniumRemotePort) {
		int oldSeleniumRemotePort = seleniumRemotePort;
		seleniumRemotePort = newSeleniumRemotePort;
		boolean oldSeleniumRemotePortESet = seleniumRemotePortESet;
		seleniumRemotePortESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT, oldSeleniumRemotePort, seleniumRemotePort, !oldSeleniumRemotePortESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetSeleniumRemotePort() {
		int oldSeleniumRemotePort = seleniumRemotePort;
		boolean oldSeleniumRemotePortESet = seleniumRemotePortESet;
		seleniumRemotePort = SELENIUM_REMOTE_PORT_EDEFAULT;
		seleniumRemotePortESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT, oldSeleniumRemotePort, SELENIUM_REMOTE_PORT_EDEFAULT, oldSeleniumRemotePortESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetSeleniumRemotePort() {
		return seleniumRemotePortESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getSeleniumBrowserConfig() {
		return seleniumBrowserConfig;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSeleniumBrowserConfig(String newSeleniumBrowserConfig) {
		String oldSeleniumBrowserConfig = seleniumBrowserConfig;
		seleniumBrowserConfig = newSeleniumBrowserConfig;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG, oldSeleniumBrowserConfig, seleniumBrowserConfig));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GAParameterType getGaParam() {
		return gaParam;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetGaParam(GAParameterType newGaParam, NotificationChain msgs) {
		GAParameterType oldGaParam = gaParam;
		gaParam = newGaParam;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM, oldGaParam, newGaParam);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGaParam(GAParameterType newGaParam) {
		if (newGaParam != gaParam) {
			NotificationChain msgs = null;
			if (gaParam != null)
				msgs = ((InternalEObject)gaParam).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM, null, msgs);
			if (newGaParam != null)
				msgs = ((InternalEObject)newGaParam).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM, null, msgs);
			msgs = basicSetGaParam(newGaParam, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM, newGaParam, newGaParam));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM:
				return basicSetGaParam(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.TEST_GENERATION_TYPE__CTE_FOLDER:
				return getCteFolder();
			case ProjectPackage.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY:
				return getModelVisitStrategy();
			case ProjectPackage.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE:
				return isReduceTestSuite() ? Boolean.TRUE : Boolean.FALSE;
			case ProjectPackage.TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX:
				return getSourcePackagePrefix();
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER:
				return getSeleniumDriverBrowser();
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST:
				return getSeleniumRemoteHost();
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT:
				return new Integer(getSeleniumRemotePort());
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG:
				return getSeleniumBrowserConfig();
			case ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM:
				return getGaParam();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ProjectPackage.TEST_GENERATION_TYPE__CTE_FOLDER:
				setCteFolder((String)newValue);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY:
				setModelVisitStrategy((ModelVisitStrategyType)newValue);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE:
				setReduceTestSuite(((Boolean)newValue).booleanValue());
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX:
				setSourcePackagePrefix((String)newValue);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER:
				setSeleniumDriverBrowser((BrowserType)newValue);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST:
				setSeleniumRemoteHost((String)newValue);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT:
				setSeleniumRemotePort(((Integer)newValue).intValue());
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG:
				setSeleniumBrowserConfig((String)newValue);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM:
				setGaParam((GAParameterType)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eUnset(int featureID) {
		switch (featureID) {
			case ProjectPackage.TEST_GENERATION_TYPE__CTE_FOLDER:
				setCteFolder(CTE_FOLDER_EDEFAULT);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY:
				unsetModelVisitStrategy();
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE:
				unsetReduceTestSuite();
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX:
				setSourcePackagePrefix(SOURCE_PACKAGE_PREFIX_EDEFAULT);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER:
				unsetSeleniumDriverBrowser();
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST:
				setSeleniumRemoteHost(SELENIUM_REMOTE_HOST_EDEFAULT);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT:
				unsetSeleniumRemotePort();
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG:
				setSeleniumBrowserConfig(SELENIUM_BROWSER_CONFIG_EDEFAULT);
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM:
				setGaParam((GAParameterType)null);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ProjectPackage.TEST_GENERATION_TYPE__CTE_FOLDER:
				return CTE_FOLDER_EDEFAULT == null ? cteFolder != null : !CTE_FOLDER_EDEFAULT.equals(cteFolder);
			case ProjectPackage.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY:
				return isSetModelVisitStrategy();
			case ProjectPackage.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE:
				return isSetReduceTestSuite();
			case ProjectPackage.TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX:
				return SOURCE_PACKAGE_PREFIX_EDEFAULT == null ? sourcePackagePrefix != null : !SOURCE_PACKAGE_PREFIX_EDEFAULT.equals(sourcePackagePrefix);
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER:
				return isSetSeleniumDriverBrowser();
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST:
				return SELENIUM_REMOTE_HOST_EDEFAULT == null ? seleniumRemoteHost != null : !SELENIUM_REMOTE_HOST_EDEFAULT.equals(seleniumRemoteHost);
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT:
				return isSetSeleniumRemotePort();
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG:
				return SELENIUM_BROWSER_CONFIG_EDEFAULT == null ? seleniumBrowserConfig != null : !SELENIUM_BROWSER_CONFIG_EDEFAULT.equals(seleniumBrowserConfig);
			case ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM:
				return gaParam != null;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (cteFolder: ");
		result.append(cteFolder);
		result.append(", modelVisitStrategy: ");
		if (modelVisitStrategyESet) result.append(modelVisitStrategy); else result.append("<unset>");
		result.append(", reduceTestSuite: ");
		if (reduceTestSuiteESet) result.append(reduceTestSuite); else result.append("<unset>");
		result.append(", sourcePackagePrefix: ");
		result.append(sourcePackagePrefix);
		result.append(", seleniumDriverBrowser: ");
		if (seleniumDriverBrowserESet) result.append(seleniumDriverBrowser); else result.append("<unset>");
		result.append(", seleniumRemoteHost: ");
		result.append(seleniumRemoteHost);
		result.append(", seleniumRemotePort: ");
		if (seleniumRemotePortESet) result.append(seleniumRemotePort); else result.append("<unset>");
		result.append(", seleniumBrowserConfig: ");
		result.append(seleniumBrowserConfig);
		result.append(')');
		return result.toString();
	}

} //TestGenerationTypeImpl
