/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage
 * @generated
 */
public interface ProjectFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	ProjectFactory eINSTANCE = eu.fittest.test.project.impl.ProjectFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Document Root</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Document Root</em>'.
	 * @generated
	 */
	DocumentRoot createDocumentRoot();

	/**
	 * Returns a new object of class '<em>GA Parameter Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>GA Parameter Type</em>'.
	 * @generated
	 */
	GAParameterType createGAParameterType();

	/**
	 * Returns a new object of class '<em>General Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>General Type</em>'.
	 * @generated
	 */
	GeneralType createGeneralType();

	/**
	 * Returns a new object of class '<em>Instrumentation Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Instrumentation Type</em>'.
	 * @generated
	 */
	InstrumentationType createInstrumentationType();

	/**
	 * Returns a new object of class '<em>Logging Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Logging Type</em>'.
	 * @generated
	 */
	LoggingType createLoggingType();

	/**
	 * Returns a new object of class '<em>Log Target Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Log Target Type</em>'.
	 * @generated
	 */
	LogTargetType createLogTargetType();

	/**
	 * Returns a new object of class '<em>Model Inference Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Model Inference Type</em>'.
	 * @generated
	 */
	ModelInferenceType createModelInferenceType();

	/**
	 * Returns a new object of class '<em>Oracle Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Oracle Type</em>'.
	 * @generated
	 */
	OracleType createOracleType();

	/**
	 * Returns a new object of class '<em>Test Generation Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Test Generation Type</em>'.
	 * @generated
	 */
	TestGenerationType createTestGenerationType();

	/**
	 * Returns a new object of class '<em>Test Project Type</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Test Project Type</em>'.
	 * @generated
	 */
	TestProjectType createTestProjectType();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	ProjectPackage getProjectPackage();

} //ProjectFactory
