/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Instrumentation Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.InstrumentationType#getGhcrtOption <em>Ghcrt Option</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getInstrumentationType()
 * @model extendedMetaData="name='InstrumentationType' kind='elementOnly'"
 * @generated
 */
public interface InstrumentationType extends EObject {
	/**
	 * Returns the value of the '<em><b>Ghcrt Option</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ghcrt Option</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ghcrt Option</em>' attribute.
	 * @see #setGhcrtOption(String)
	 * @see eu.fittest.test.project.ProjectPackage#getInstrumentationType_GhcrtOption()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='ghcrtOption' namespace='##targetNamespace'"
	 * @generated
	 */
	String getGhcrtOption();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.InstrumentationType#getGhcrtOption <em>Ghcrt Option</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ghcrt Option</em>' attribute.
	 * @see #getGhcrtOption()
	 * @generated
	 */
	void setGhcrtOption(String value);

} // InstrumentationType
