/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Log Target Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.LogTargetType#getStoreDir <em>Store Dir</em>}</li>
 *   <li>{@link eu.fittest.test.project.LogTargetType#getLogLevel <em>Log Level</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getLogTargetType()
 * @model extendedMetaData="name='LogTargetType' kind='elementOnly'"
 * @generated
 */
public interface LogTargetType extends EObject {
	/**
	 * Returns the value of the '<em><b>Store Dir</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Store Dir</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Store Dir</em>' attribute.
	 * @see #setStoreDir(String)
	 * @see eu.fittest.test.project.ProjectPackage#getLogTargetType_StoreDir()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='storeDir' namespace='##targetNamespace'"
	 * @generated
	 */
	String getStoreDir();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.LogTargetType#getStoreDir <em>Store Dir</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Store Dir</em>' attribute.
	 * @see #getStoreDir()
	 * @generated
	 */
	void setStoreDir(String value);

	/**
	 * Returns the value of the '<em><b>Log Level</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Log Level</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Log Level</em>' attribute.
	 * @see #isSetLogLevel()
	 * @see #unsetLogLevel()
	 * @see #setLogLevel(int)
	 * @see eu.fittest.test.project.ProjectPackage#getLogTargetType_LogLevel()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='logLevel' namespace='##targetNamespace'"
	 * @generated
	 */
	int getLogLevel();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.LogTargetType#getLogLevel <em>Log Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Log Level</em>' attribute.
	 * @see #isSetLogLevel()
	 * @see #unsetLogLevel()
	 * @see #getLogLevel()
	 * @generated
	 */
	void setLogLevel(int value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.LogTargetType#getLogLevel <em>Log Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetLogLevel()
	 * @see #getLogLevel()
	 * @see #setLogLevel(int)
	 * @generated
	 */
	void unsetLogLevel();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.LogTargetType#getLogLevel <em>Log Level</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Log Level</em>' attribute is set.
	 * @see #unsetLogLevel()
	 * @see #getLogLevel()
	 * @see #setLogLevel(int)
	 * @generated
	 */
	boolean isSetLogLevel();

} // LogTargetType
