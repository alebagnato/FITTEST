/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Oracle Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.OracleType#getGHCRTopts <em>GHCR Topts</em>}</li>
 *   <li>{@link eu.fittest.test.project.OracleType#getOracleFile <em>Oracle File</em>}</li>
 *   <li>{@link eu.fittest.test.project.OracleType#getReportFile <em>Report File</em>}</li>
 *   <li>{@link eu.fittest.test.project.OracleType#getEventsToInclude <em>Events To Include</em>}</li>
 *   <li>{@link eu.fittest.test.project.OracleType#getFieldsToInclude <em>Fields To Include</em>}</li>
 *   <li>{@link eu.fittest.test.project.OracleType#getFunctionsToInclude <em>Functions To Include</em>}</li>
 *   <li>{@link eu.fittest.test.project.OracleType#getLloOption <em>Llo Option</em>}</li>
 *   <li>{@link eu.fittest.test.project.OracleType#getViolationFile <em>Violation File</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getOracleType()
 * @model extendedMetaData="name='OracleType' kind='elementOnly'"
 * @generated
 */
public interface OracleType extends EObject {
	/**
	 * Returns the value of the '<em><b>GHCR Topts</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>GHCR Topts</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>GHCR Topts</em>' attribute.
	 * @see #setGHCRTopts(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_GHCRTopts()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='GHCRTopts' namespace='##targetNamespace'"
	 * @generated
	 */
	String getGHCRTopts();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getGHCRTopts <em>GHCR Topts</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>GHCR Topts</em>' attribute.
	 * @see #getGHCRTopts()
	 * @generated
	 */
	void setGHCRTopts(String value);

	/**
	 * Returns the value of the '<em><b>Oracle File</b></em>' attribute.
	 * The default value is <code>"oracle.inv"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Oracle File</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Oracle File</em>' attribute.
	 * @see #isSetOracleFile()
	 * @see #unsetOracleFile()
	 * @see #setOracleFile(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_OracleFile()
	 * @model default="oracle.inv" unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='oracleFile' namespace='##targetNamespace'"
	 * @generated
	 */
	String getOracleFile();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getOracleFile <em>Oracle File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Oracle File</em>' attribute.
	 * @see #isSetOracleFile()
	 * @see #unsetOracleFile()
	 * @see #getOracleFile()
	 * @generated
	 */
	void setOracleFile(String value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.OracleType#getOracleFile <em>Oracle File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetOracleFile()
	 * @see #getOracleFile()
	 * @see #setOracleFile(String)
	 * @generated
	 */
	void unsetOracleFile();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.OracleType#getOracleFile <em>Oracle File</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Oracle File</em>' attribute is set.
	 * @see #unsetOracleFile()
	 * @see #getOracleFile()
	 * @see #setOracleFile(String)
	 * @generated
	 */
	boolean isSetOracleFile();

	/**
	 * Returns the value of the '<em><b>Report File</b></em>' attribute.
	 * The default value is <code>"report.txt"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Report File</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Report File</em>' attribute.
	 * @see #isSetReportFile()
	 * @see #unsetReportFile()
	 * @see #setReportFile(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_ReportFile()
	 * @model default="report.txt" unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='reportFile' namespace='##targetNamespace'"
	 * @generated
	 */
	String getReportFile();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getReportFile <em>Report File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Report File</em>' attribute.
	 * @see #isSetReportFile()
	 * @see #unsetReportFile()
	 * @see #getReportFile()
	 * @generated
	 */
	void setReportFile(String value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.OracleType#getReportFile <em>Report File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetReportFile()
	 * @see #getReportFile()
	 * @see #setReportFile(String)
	 * @generated
	 */
	void unsetReportFile();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.OracleType#getReportFile <em>Report File</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Report File</em>' attribute is set.
	 * @see #unsetReportFile()
	 * @see #getReportFile()
	 * @see #setReportFile(String)
	 * @generated
	 */
	boolean isSetReportFile();

	/**
	 * Returns the value of the '<em><b>Events To Include</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Events To Include</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Events To Include</em>' attribute.
	 * @see #setEventsToInclude(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_EventsToInclude()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='eventsToInclude' namespace='##targetNamespace'"
	 * @generated
	 */
	String getEventsToInclude();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getEventsToInclude <em>Events To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Events To Include</em>' attribute.
	 * @see #getEventsToInclude()
	 * @generated
	 */
	void setEventsToInclude(String value);

	/**
	 * Returns the value of the '<em><b>Fields To Include</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Fields To Include</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Fields To Include</em>' attribute.
	 * @see #setFieldsToInclude(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_FieldsToInclude()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='fieldsToInclude' namespace='##targetNamespace'"
	 * @generated
	 */
	String getFieldsToInclude();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getFieldsToInclude <em>Fields To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Fields To Include</em>' attribute.
	 * @see #getFieldsToInclude()
	 * @generated
	 */
	void setFieldsToInclude(String value);

	/**
	 * Returns the value of the '<em><b>Functions To Include</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Functions To Include</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Functions To Include</em>' attribute.
	 * @see #setFunctionsToInclude(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_FunctionsToInclude()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='functionsToInclude' namespace='##targetNamespace'"
	 * @generated
	 */
	String getFunctionsToInclude();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getFunctionsToInclude <em>Functions To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Functions To Include</em>' attribute.
	 * @see #getFunctionsToInclude()
	 * @generated
	 */
	void setFunctionsToInclude(String value);

	/**
	 * Returns the value of the '<em><b>Llo Option</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Llo Option</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Llo Option</em>' attribute.
	 * @see #setLloOption(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_LloOption()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='lloOption' namespace='##targetNamespace'"
	 * @generated
	 */
	String getLloOption();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getLloOption <em>Llo Option</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Llo Option</em>' attribute.
	 * @see #getLloOption()
	 * @generated
	 */
	void setLloOption(String value);

	/**
	 * Returns the value of the '<em><b>Violation File</b></em>' attribute.
	 * The default value is <code>"violations.txt"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Violation File</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Violation File</em>' attribute.
	 * @see #isSetViolationFile()
	 * @see #unsetViolationFile()
	 * @see #setViolationFile(String)
	 * @see eu.fittest.test.project.ProjectPackage#getOracleType_ViolationFile()
	 * @model default="violations.txt" unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='violationFile' namespace='##targetNamespace'"
	 * @generated
	 */
	String getViolationFile();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.OracleType#getViolationFile <em>Violation File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Violation File</em>' attribute.
	 * @see #isSetViolationFile()
	 * @see #unsetViolationFile()
	 * @see #getViolationFile()
	 * @generated
	 */
	void setViolationFile(String value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.OracleType#getViolationFile <em>Violation File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetViolationFile()
	 * @see #getViolationFile()
	 * @see #setViolationFile(String)
	 * @generated
	 */
	void unsetViolationFile();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.OracleType#getViolationFile <em>Violation File</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Violation File</em>' attribute is set.
	 * @see #unsetViolationFile()
	 * @see #getViolationFile()
	 * @see #setViolationFile(String)
	 * @generated
	 */
	boolean isSetViolationFile();

} // OracleType
