/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Test Generation Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getCteFolder <em>Cte Folder</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getModelVisitStrategy <em>Model Visit Strategy</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#isReduceTestSuite <em>Reduce Test Suite</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getSourcePackagePrefix <em>Source Package Prefix</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getSeleniumDriverBrowser <em>Selenium Driver Browser</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemoteHost <em>Selenium Remote Host</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemotePort <em>Selenium Remote Port</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getSeleniumBrowserConfig <em>Selenium Browser Config</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestGenerationType#getGaParam <em>Ga Param</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType()
 * @model extendedMetaData="name='TestGenerationType' kind='elementOnly'"
 * @generated
 */
public interface TestGenerationType extends EObject {
	/**
	 * Returns the value of the '<em><b>Cte Folder</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cte Folder</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Cte Folder</em>' attribute.
	 * @see #setCteFolder(String)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_CteFolder()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='cteFolder' namespace='##targetNamespace'"
	 * @generated
	 */
	String getCteFolder();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getCteFolder <em>Cte Folder</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Cte Folder</em>' attribute.
	 * @see #getCteFolder()
	 * @generated
	 */
	void setCteFolder(String value);

	/**
	 * Returns the value of the '<em><b>Model Visit Strategy</b></em>' attribute.
	 * The literals are from the enumeration {@link eu.fittest.test.project.ModelVisitStrategyType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Model Visit Strategy</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Model Visit Strategy</em>' attribute.
	 * @see eu.fittest.test.project.ModelVisitStrategyType
	 * @see #isSetModelVisitStrategy()
	 * @see #unsetModelVisitStrategy()
	 * @see #setModelVisitStrategy(ModelVisitStrategyType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_ModelVisitStrategy()
	 * @model unsettable="true" required="true"
	 *        extendedMetaData="kind='element' name='modelVisitStrategy' namespace='##targetNamespace'"
	 * @generated
	 */
	ModelVisitStrategyType getModelVisitStrategy();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getModelVisitStrategy <em>Model Visit Strategy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Model Visit Strategy</em>' attribute.
	 * @see eu.fittest.test.project.ModelVisitStrategyType
	 * @see #isSetModelVisitStrategy()
	 * @see #unsetModelVisitStrategy()
	 * @see #getModelVisitStrategy()
	 * @generated
	 */
	void setModelVisitStrategy(ModelVisitStrategyType value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.TestGenerationType#getModelVisitStrategy <em>Model Visit Strategy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetModelVisitStrategy()
	 * @see #getModelVisitStrategy()
	 * @see #setModelVisitStrategy(ModelVisitStrategyType)
	 * @generated
	 */
	void unsetModelVisitStrategy();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.TestGenerationType#getModelVisitStrategy <em>Model Visit Strategy</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Model Visit Strategy</em>' attribute is set.
	 * @see #unsetModelVisitStrategy()
	 * @see #getModelVisitStrategy()
	 * @see #setModelVisitStrategy(ModelVisitStrategyType)
	 * @generated
	 */
	boolean isSetModelVisitStrategy();

	/**
	 * Returns the value of the '<em><b>Reduce Test Suite</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reduce Test Suite</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Reduce Test Suite</em>' attribute.
	 * @see #isSetReduceTestSuite()
	 * @see #unsetReduceTestSuite()
	 * @see #setReduceTestSuite(boolean)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_ReduceTestSuite()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Boolean" required="true"
	 *        extendedMetaData="kind='element' name='reduceTestSuite' namespace='##targetNamespace'"
	 * @generated
	 */
	boolean isReduceTestSuite();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#isReduceTestSuite <em>Reduce Test Suite</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Reduce Test Suite</em>' attribute.
	 * @see #isSetReduceTestSuite()
	 * @see #unsetReduceTestSuite()
	 * @see #isReduceTestSuite()
	 * @generated
	 */
	void setReduceTestSuite(boolean value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.TestGenerationType#isReduceTestSuite <em>Reduce Test Suite</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetReduceTestSuite()
	 * @see #isReduceTestSuite()
	 * @see #setReduceTestSuite(boolean)
	 * @generated
	 */
	void unsetReduceTestSuite();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.TestGenerationType#isReduceTestSuite <em>Reduce Test Suite</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Reduce Test Suite</em>' attribute is set.
	 * @see #unsetReduceTestSuite()
	 * @see #isReduceTestSuite()
	 * @see #setReduceTestSuite(boolean)
	 * @generated
	 */
	boolean isSetReduceTestSuite();

	/**
	 * Returns the value of the '<em><b>Source Package Prefix</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Source Package Prefix</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Source Package Prefix</em>' attribute.
	 * @see #setSourcePackagePrefix(String)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_SourcePackagePrefix()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='sourcePackagePrefix' namespace='##targetNamespace'"
	 * @generated
	 */
	String getSourcePackagePrefix();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getSourcePackagePrefix <em>Source Package Prefix</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Source Package Prefix</em>' attribute.
	 * @see #getSourcePackagePrefix()
	 * @generated
	 */
	void setSourcePackagePrefix(String value);

	/**
	 * Returns the value of the '<em><b>Selenium Driver Browser</b></em>' attribute.
	 * The literals are from the enumeration {@link eu.fittest.test.project.BrowserType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selenium Driver Browser</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selenium Driver Browser</em>' attribute.
	 * @see eu.fittest.test.project.BrowserType
	 * @see #isSetSeleniumDriverBrowser()
	 * @see #unsetSeleniumDriverBrowser()
	 * @see #setSeleniumDriverBrowser(BrowserType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_SeleniumDriverBrowser()
	 * @model unsettable="true" required="true"
	 *        extendedMetaData="kind='element' name='seleniumDriverBrowser' namespace='##targetNamespace'"
	 * @generated
	 */
	BrowserType getSeleniumDriverBrowser();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumDriverBrowser <em>Selenium Driver Browser</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Selenium Driver Browser</em>' attribute.
	 * @see eu.fittest.test.project.BrowserType
	 * @see #isSetSeleniumDriverBrowser()
	 * @see #unsetSeleniumDriverBrowser()
	 * @see #getSeleniumDriverBrowser()
	 * @generated
	 */
	void setSeleniumDriverBrowser(BrowserType value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumDriverBrowser <em>Selenium Driver Browser</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetSeleniumDriverBrowser()
	 * @see #getSeleniumDriverBrowser()
	 * @see #setSeleniumDriverBrowser(BrowserType)
	 * @generated
	 */
	void unsetSeleniumDriverBrowser();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumDriverBrowser <em>Selenium Driver Browser</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Selenium Driver Browser</em>' attribute is set.
	 * @see #unsetSeleniumDriverBrowser()
	 * @see #getSeleniumDriverBrowser()
	 * @see #setSeleniumDriverBrowser(BrowserType)
	 * @generated
	 */
	boolean isSetSeleniumDriverBrowser();

	/**
	 * Returns the value of the '<em><b>Selenium Remote Host</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selenium Remote Host</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selenium Remote Host</em>' attribute.
	 * @see #setSeleniumRemoteHost(String)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_SeleniumRemoteHost()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='seleniumRemoteHost' namespace='##targetNamespace'"
	 * @generated
	 */
	String getSeleniumRemoteHost();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemoteHost <em>Selenium Remote Host</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Selenium Remote Host</em>' attribute.
	 * @see #getSeleniumRemoteHost()
	 * @generated
	 */
	void setSeleniumRemoteHost(String value);

	/**
	 * Returns the value of the '<em><b>Selenium Remote Port</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selenium Remote Port</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selenium Remote Port</em>' attribute.
	 * @see #isSetSeleniumRemotePort()
	 * @see #unsetSeleniumRemotePort()
	 * @see #setSeleniumRemotePort(int)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_SeleniumRemotePort()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='seleniumRemotePort' namespace='##targetNamespace'"
	 * @generated
	 */
	int getSeleniumRemotePort();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemotePort <em>Selenium Remote Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Selenium Remote Port</em>' attribute.
	 * @see #isSetSeleniumRemotePort()
	 * @see #unsetSeleniumRemotePort()
	 * @see #getSeleniumRemotePort()
	 * @generated
	 */
	void setSeleniumRemotePort(int value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemotePort <em>Selenium Remote Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetSeleniumRemotePort()
	 * @see #getSeleniumRemotePort()
	 * @see #setSeleniumRemotePort(int)
	 * @generated
	 */
	void unsetSeleniumRemotePort();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumRemotePort <em>Selenium Remote Port</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Selenium Remote Port</em>' attribute is set.
	 * @see #unsetSeleniumRemotePort()
	 * @see #getSeleniumRemotePort()
	 * @see #setSeleniumRemotePort(int)
	 * @generated
	 */
	boolean isSetSeleniumRemotePort();

	/**
	 * Returns the value of the '<em><b>Selenium Browser Config</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selenium Browser Config</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selenium Browser Config</em>' attribute.
	 * @see #setSeleniumBrowserConfig(String)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_SeleniumBrowserConfig()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='seleniumBrowserConfig' namespace='##targetNamespace'"
	 * @generated
	 */
	String getSeleniumBrowserConfig();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getSeleniumBrowserConfig <em>Selenium Browser Config</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Selenium Browser Config</em>' attribute.
	 * @see #getSeleniumBrowserConfig()
	 * @generated
	 */
	void setSeleniumBrowserConfig(String value);

	/**
	 * Returns the value of the '<em><b>Ga Param</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ga Param</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ga Param</em>' containment reference.
	 * @see #setGaParam(GAParameterType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestGenerationType_GaParam()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='gaParam' namespace='##targetNamespace'"
	 * @generated
	 */
	GAParameterType getGaParam();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestGenerationType#getGaParam <em>Ga Param</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ga Param</em>' containment reference.
	 * @see #getGaParam()
	 * @generated
	 */
	void setGaParam(GAParameterType value);

} // TestGenerationType
