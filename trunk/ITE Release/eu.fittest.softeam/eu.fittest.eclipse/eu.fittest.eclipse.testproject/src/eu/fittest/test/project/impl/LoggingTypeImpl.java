/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.InstrumentationType;
import eu.fittest.test.project.LogTargetType;
import eu.fittest.test.project.LoggingType;
import eu.fittest.test.project.ProjectPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Logging Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.LoggingTypeImpl#getInstrumentation <em>Instrumentation</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.LoggingTypeImpl#getLogTarget <em>Log Target</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class LoggingTypeImpl extends EObjectImpl implements LoggingType {
	/**
	 * The cached value of the '{@link #getInstrumentation() <em>Instrumentation</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInstrumentation()
	 * @generated
	 * @ordered
	 */
	protected InstrumentationType instrumentation;

	/**
	 * The cached value of the '{@link #getLogTarget() <em>Log Target</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLogTarget()
	 * @generated
	 * @ordered
	 */
	protected LogTargetType logTarget;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected LoggingTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.LOGGING_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InstrumentationType getInstrumentation() {
		return instrumentation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetInstrumentation(InstrumentationType newInstrumentation, NotificationChain msgs) {
		InstrumentationType oldInstrumentation = instrumentation;
		instrumentation = newInstrumentation;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.LOGGING_TYPE__INSTRUMENTATION, oldInstrumentation, newInstrumentation);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setInstrumentation(InstrumentationType newInstrumentation) {
		if (newInstrumentation != instrumentation) {
			NotificationChain msgs = null;
			if (instrumentation != null)
				msgs = ((InternalEObject)instrumentation).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.LOGGING_TYPE__INSTRUMENTATION, null, msgs);
			if (newInstrumentation != null)
				msgs = ((InternalEObject)newInstrumentation).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.LOGGING_TYPE__INSTRUMENTATION, null, msgs);
			msgs = basicSetInstrumentation(newInstrumentation, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.LOGGING_TYPE__INSTRUMENTATION, newInstrumentation, newInstrumentation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LogTargetType getLogTarget() {
		return logTarget;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetLogTarget(LogTargetType newLogTarget, NotificationChain msgs) {
		LogTargetType oldLogTarget = logTarget;
		logTarget = newLogTarget;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.LOGGING_TYPE__LOG_TARGET, oldLogTarget, newLogTarget);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLogTarget(LogTargetType newLogTarget) {
		if (newLogTarget != logTarget) {
			NotificationChain msgs = null;
			if (logTarget != null)
				msgs = ((InternalEObject)logTarget).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.LOGGING_TYPE__LOG_TARGET, null, msgs);
			if (newLogTarget != null)
				msgs = ((InternalEObject)newLogTarget).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.LOGGING_TYPE__LOG_TARGET, null, msgs);
			msgs = basicSetLogTarget(newLogTarget, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.LOGGING_TYPE__LOG_TARGET, newLogTarget, newLogTarget));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ProjectPackage.LOGGING_TYPE__INSTRUMENTATION:
				return basicSetInstrumentation(null, msgs);
			case ProjectPackage.LOGGING_TYPE__LOG_TARGET:
				return basicSetLogTarget(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.LOGGING_TYPE__INSTRUMENTATION:
				return getInstrumentation();
			case ProjectPackage.LOGGING_TYPE__LOG_TARGET:
				return getLogTarget();
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
			case ProjectPackage.LOGGING_TYPE__INSTRUMENTATION:
				setInstrumentation((InstrumentationType)newValue);
				return;
			case ProjectPackage.LOGGING_TYPE__LOG_TARGET:
				setLogTarget((LogTargetType)newValue);
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
			case ProjectPackage.LOGGING_TYPE__INSTRUMENTATION:
				setInstrumentation((InstrumentationType)null);
				return;
			case ProjectPackage.LOGGING_TYPE__LOG_TARGET:
				setLogTarget((LogTargetType)null);
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
			case ProjectPackage.LOGGING_TYPE__INSTRUMENTATION:
				return instrumentation != null;
			case ProjectPackage.LOGGING_TYPE__LOG_TARGET:
				return logTarget != null;
		}
		return super.eIsSet(featureID);
	}

} //LoggingTypeImpl
