/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.LogTargetType;
import eu.fittest.test.project.ProjectPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Log Target Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.LogTargetTypeImpl#getStoreDir <em>Store Dir</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.LogTargetTypeImpl#getLogLevel <em>Log Level</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class LogTargetTypeImpl extends EObjectImpl implements LogTargetType {
	/**
	 * The default value of the '{@link #getStoreDir() <em>Store Dir</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStoreDir()
	 * @generated
	 * @ordered
	 */
	protected static final String STORE_DIR_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getStoreDir() <em>Store Dir</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStoreDir()
	 * @generated
	 * @ordered
	 */
	protected String storeDir = STORE_DIR_EDEFAULT;

	/**
	 * The default value of the '{@link #getLogLevel() <em>Log Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLogLevel()
	 * @generated
	 * @ordered
	 */
	protected static final int LOG_LEVEL_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getLogLevel() <em>Log Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLogLevel()
	 * @generated
	 * @ordered
	 */
	protected int logLevel = LOG_LEVEL_EDEFAULT;

	/**
	 * This is true if the Log Level attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean logLevelESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected LogTargetTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.LOG_TARGET_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getStoreDir() {
		return storeDir;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStoreDir(String newStoreDir) {
		String oldStoreDir = storeDir;
		storeDir = newStoreDir;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.LOG_TARGET_TYPE__STORE_DIR, oldStoreDir, storeDir));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getLogLevel() {
		return logLevel;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLogLevel(int newLogLevel) {
		int oldLogLevel = logLevel;
		logLevel = newLogLevel;
		boolean oldLogLevelESet = logLevelESet;
		logLevelESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.LOG_TARGET_TYPE__LOG_LEVEL, oldLogLevel, logLevel, !oldLogLevelESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetLogLevel() {
		int oldLogLevel = logLevel;
		boolean oldLogLevelESet = logLevelESet;
		logLevel = LOG_LEVEL_EDEFAULT;
		logLevelESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.LOG_TARGET_TYPE__LOG_LEVEL, oldLogLevel, LOG_LEVEL_EDEFAULT, oldLogLevelESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetLogLevel() {
		return logLevelESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.LOG_TARGET_TYPE__STORE_DIR:
				return getStoreDir();
			case ProjectPackage.LOG_TARGET_TYPE__LOG_LEVEL:
				return new Integer(getLogLevel());
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
			case ProjectPackage.LOG_TARGET_TYPE__STORE_DIR:
				setStoreDir((String)newValue);
				return;
			case ProjectPackage.LOG_TARGET_TYPE__LOG_LEVEL:
				setLogLevel(((Integer)newValue).intValue());
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
			case ProjectPackage.LOG_TARGET_TYPE__STORE_DIR:
				setStoreDir(STORE_DIR_EDEFAULT);
				return;
			case ProjectPackage.LOG_TARGET_TYPE__LOG_LEVEL:
				unsetLogLevel();
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
			case ProjectPackage.LOG_TARGET_TYPE__STORE_DIR:
				return STORE_DIR_EDEFAULT == null ? storeDir != null : !STORE_DIR_EDEFAULT.equals(storeDir);
			case ProjectPackage.LOG_TARGET_TYPE__LOG_LEVEL:
				return isSetLogLevel();
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
		result.append(" (storeDir: ");
		result.append(storeDir);
		result.append(", logLevel: ");
		if (logLevelESet) result.append(logLevel); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //LogTargetTypeImpl
