/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Logging Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.LoggingType#getInstrumentation <em>Instrumentation</em>}</li>
 *   <li>{@link eu.fittest.test.project.LoggingType#getLogTarget <em>Log Target</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getLoggingType()
 * @model extendedMetaData="name='LoggingType' kind='elementOnly'"
 * @generated
 */
public interface LoggingType extends EObject {
	/**
	 * Returns the value of the '<em><b>Instrumentation</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Instrumentation</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Instrumentation</em>' containment reference.
	 * @see #setInstrumentation(InstrumentationType)
	 * @see eu.fittest.test.project.ProjectPackage#getLoggingType_Instrumentation()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='Instrumentation' namespace='##targetNamespace'"
	 * @generated
	 */
	InstrumentationType getInstrumentation();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.LoggingType#getInstrumentation <em>Instrumentation</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Instrumentation</em>' containment reference.
	 * @see #getInstrumentation()
	 * @generated
	 */
	void setInstrumentation(InstrumentationType value);

	/**
	 * Returns the value of the '<em><b>Log Target</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Log Target</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Log Target</em>' containment reference.
	 * @see #setLogTarget(LogTargetType)
	 * @see eu.fittest.test.project.ProjectPackage#getLoggingType_LogTarget()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='LogTarget' namespace='##targetNamespace'"
	 * @generated
	 */
	LogTargetType getLogTarget();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.LoggingType#getLogTarget <em>Log Target</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Log Target</em>' containment reference.
	 * @see #getLogTarget()
	 * @generated
	 */
	void setLogTarget(LogTargetType value);

} // LoggingType
