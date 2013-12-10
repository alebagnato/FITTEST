/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>General Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.GeneralType#getType <em>Type</em>}</li>
 *   <li>{@link eu.fittest.test.project.GeneralType#getBaseURL <em>Base URL</em>}</li>
 *   <li>{@link eu.fittest.test.project.GeneralType#getEntryPage <em>Entry Page</em>}</li>
 *   <li>{@link eu.fittest.test.project.GeneralType#getServerFolder <em>Server Folder</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getGeneralType()
 * @model extendedMetaData="name='GeneralType' kind='elementOnly'"
 * @generated
 */
public interface GeneralType extends EObject {
	/**
	 * Returns the value of the '<em><b>Type</b></em>' attribute.
	 * The default value is <code>"FLASH"</code>.
	 * The literals are from the enumeration {@link eu.fittest.test.project.SUTTechnologyType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' attribute.
	 * @see eu.fittest.test.project.SUTTechnologyType
	 * @see #isSetType()
	 * @see #unsetType()
	 * @see #setType(SUTTechnologyType)
	 * @see eu.fittest.test.project.ProjectPackage#getGeneralType_Type()
	 * @model default="FLASH" unsettable="true" required="true"
	 *        extendedMetaData="kind='element' name='type' namespace='##targetNamespace'"
	 * @generated
	 */
	SUTTechnologyType getType();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GeneralType#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' attribute.
	 * @see eu.fittest.test.project.SUTTechnologyType
	 * @see #isSetType()
	 * @see #unsetType()
	 * @see #getType()
	 * @generated
	 */
	void setType(SUTTechnologyType value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.GeneralType#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetType()
	 * @see #getType()
	 * @see #setType(SUTTechnologyType)
	 * @generated
	 */
	void unsetType();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.GeneralType#getType <em>Type</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Type</em>' attribute is set.
	 * @see #unsetType()
	 * @see #getType()
	 * @see #setType(SUTTechnologyType)
	 * @generated
	 */
	boolean isSetType();

	/**
	 * Returns the value of the '<em><b>Base URL</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base URL</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base URL</em>' attribute.
	 * @see #setBaseURL(String)
	 * @see eu.fittest.test.project.ProjectPackage#getGeneralType_BaseURL()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='baseURL' namespace='##targetNamespace'"
	 * @generated
	 */
	String getBaseURL();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GeneralType#getBaseURL <em>Base URL</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base URL</em>' attribute.
	 * @see #getBaseURL()
	 * @generated
	 */
	void setBaseURL(String value);

	/**
	 * Returns the value of the '<em><b>Entry Page</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Entry Page</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Entry Page</em>' attribute.
	 * @see #setEntryPage(String)
	 * @see eu.fittest.test.project.ProjectPackage#getGeneralType_EntryPage()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='entryPage' namespace='##targetNamespace'"
	 * @generated
	 */
	String getEntryPage();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GeneralType#getEntryPage <em>Entry Page</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Entry Page</em>' attribute.
	 * @see #getEntryPage()
	 * @generated
	 */
	void setEntryPage(String value);

	/**
	 * Returns the value of the '<em><b>Server Folder</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Server Folder</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Server Folder</em>' attribute.
	 * @see #setServerFolder(String)
	 * @see eu.fittest.test.project.ProjectPackage#getGeneralType_ServerFolder()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='element' name='serverFolder' namespace='##targetNamespace'"
	 * @generated
	 */
	String getServerFolder();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GeneralType#getServerFolder <em>Server Folder</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Server Folder</em>' attribute.
	 * @see #getServerFolder()
	 * @generated
	 */
	void setServerFolder(String value);

} // GeneralType
