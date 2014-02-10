/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Test Project Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.TestProjectType#getGeneral <em>General</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestProjectType#getLogging <em>Logging</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestProjectType#getModelInference <em>Model Inference</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestProjectType#getTestGeneration <em>Test Generation</em>}</li>
 *   <li>{@link eu.fittest.test.project.TestProjectType#getOracle <em>Oracle</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getTestProjectType()
 * @model extendedMetaData="name='TestProject_._type' kind='elementOnly'"
 * @generated
 */
public interface TestProjectType extends EObject {
	/**
	 * Returns the value of the '<em><b>General</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>General</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>General</em>' containment reference.
	 * @see #setGeneral(GeneralType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestProjectType_General()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='General' namespace='##targetNamespace'"
	 * @generated
	 */
	GeneralType getGeneral();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestProjectType#getGeneral <em>General</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>General</em>' containment reference.
	 * @see #getGeneral()
	 * @generated
	 */
	void setGeneral(GeneralType value);

	/**
	 * Returns the value of the '<em><b>Logging</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Logging</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Logging</em>' containment reference.
	 * @see #setLogging(LoggingType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestProjectType_Logging()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='Logging' namespace='##targetNamespace'"
	 * @generated
	 */
	LoggingType getLogging();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestProjectType#getLogging <em>Logging</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Logging</em>' containment reference.
	 * @see #getLogging()
	 * @generated
	 */
	void setLogging(LoggingType value);

	/**
	 * Returns the value of the '<em><b>Model Inference</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Model Inference</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Model Inference</em>' containment reference.
	 * @see #setModelInference(ModelInferenceType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestProjectType_ModelInference()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='ModelInference' namespace='##targetNamespace'"
	 * @generated
	 */
	ModelInferenceType getModelInference();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestProjectType#getModelInference <em>Model Inference</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Model Inference</em>' containment reference.
	 * @see #getModelInference()
	 * @generated
	 */
	void setModelInference(ModelInferenceType value);

	/**
	 * Returns the value of the '<em><b>Test Generation</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Test Generation</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Test Generation</em>' containment reference.
	 * @see #setTestGeneration(TestGenerationType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestProjectType_TestGeneration()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='TestGeneration' namespace='##targetNamespace'"
	 * @generated
	 */
	TestGenerationType getTestGeneration();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestProjectType#getTestGeneration <em>Test Generation</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Test Generation</em>' containment reference.
	 * @see #getTestGeneration()
	 * @generated
	 */
	void setTestGeneration(TestGenerationType value);

	/**
	 * Returns the value of the '<em><b>Oracle</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Oracle</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Oracle</em>' containment reference.
	 * @see #setOracle(OracleType)
	 * @see eu.fittest.test.project.ProjectPackage#getTestProjectType_Oracle()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='Oracle' namespace='##targetNamespace'"
	 * @generated
	 */
	OracleType getOracle();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.TestProjectType#getOracle <em>Oracle</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Oracle</em>' containment reference.
	 * @see #getOracle()
	 * @generated
	 */
	void setOracle(OracleType value);

} // TestProjectType
