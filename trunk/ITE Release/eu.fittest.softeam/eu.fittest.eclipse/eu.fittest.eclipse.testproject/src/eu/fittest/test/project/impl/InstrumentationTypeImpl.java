/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.InstrumentationType;
import eu.fittest.test.project.ProjectPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Instrumentation Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.InstrumentationTypeImpl#getGhcrtOption <em>Ghcrt Option</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InstrumentationTypeImpl extends EObjectImpl implements InstrumentationType {
	/**
	 * The default value of the '{@link #getGhcrtOption() <em>Ghcrt Option</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGhcrtOption()
	 * @generated
	 * @ordered
	 */
	protected static final String GHCRT_OPTION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getGhcrtOption() <em>Ghcrt Option</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGhcrtOption()
	 * @generated
	 * @ordered
	 */
	protected String ghcrtOption = GHCRT_OPTION_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected InstrumentationTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.INSTRUMENTATION_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getGhcrtOption() {
		return ghcrtOption;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGhcrtOption(String newGhcrtOption) {
		String oldGhcrtOption = ghcrtOption;
		ghcrtOption = newGhcrtOption;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.INSTRUMENTATION_TYPE__GHCRT_OPTION, oldGhcrtOption, ghcrtOption));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.INSTRUMENTATION_TYPE__GHCRT_OPTION:
				return getGhcrtOption();
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
			case ProjectPackage.INSTRUMENTATION_TYPE__GHCRT_OPTION:
				setGhcrtOption((String)newValue);
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
			case ProjectPackage.INSTRUMENTATION_TYPE__GHCRT_OPTION:
				setGhcrtOption(GHCRT_OPTION_EDEFAULT);
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
			case ProjectPackage.INSTRUMENTATION_TYPE__GHCRT_OPTION:
				return GHCRT_OPTION_EDEFAULT == null ? ghcrtOption != null : !GHCRT_OPTION_EDEFAULT.equals(ghcrtOption);
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
		result.append(" (ghcrtOption: ");
		result.append(ghcrtOption);
		result.append(')');
		return result.toString();
	}

} //InstrumentationTypeImpl
