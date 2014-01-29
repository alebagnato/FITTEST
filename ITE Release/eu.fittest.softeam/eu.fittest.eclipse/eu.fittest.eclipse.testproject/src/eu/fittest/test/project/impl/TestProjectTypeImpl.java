/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.GeneralType;
import eu.fittest.test.project.LoggingType;
import eu.fittest.test.project.ModelInferenceType;
import eu.fittest.test.project.OracleType;
import eu.fittest.test.project.ProjectPackage;
import eu.fittest.test.project.TestGenerationType;
import eu.fittest.test.project.TestProjectType;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Test Project Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.TestProjectTypeImpl#getGeneral <em>General</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestProjectTypeImpl#getLogging <em>Logging</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestProjectTypeImpl#getModelInference <em>Model Inference</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestProjectTypeImpl#getTestGeneration <em>Test Generation</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.TestProjectTypeImpl#getOracle <em>Oracle</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TestProjectTypeImpl extends EObjectImpl implements TestProjectType {
	/**
	 * The cached value of the '{@link #getGeneral() <em>General</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGeneral()
	 * @generated
	 * @ordered
	 */
	protected GeneralType general;

	/**
	 * The cached value of the '{@link #getLogging() <em>Logging</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLogging()
	 * @generated
	 * @ordered
	 */
	protected LoggingType logging;

	/**
	 * The cached value of the '{@link #getModelInference() <em>Model Inference</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelInference()
	 * @generated
	 * @ordered
	 */
	protected ModelInferenceType modelInference;

	/**
	 * The cached value of the '{@link #getTestGeneration() <em>Test Generation</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTestGeneration()
	 * @generated
	 * @ordered
	 */
	protected TestGenerationType testGeneration;

	/**
	 * The cached value of the '{@link #getOracle() <em>Oracle</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOracle()
	 * @generated
	 * @ordered
	 */
	protected OracleType oracle;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TestProjectTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.TEST_PROJECT_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GeneralType getGeneral() {
		return general;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetGeneral(GeneralType newGeneral, NotificationChain msgs) {
		GeneralType oldGeneral = general;
		general = newGeneral;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__GENERAL, oldGeneral, newGeneral);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGeneral(GeneralType newGeneral) {
		if (newGeneral != general) {
			NotificationChain msgs = null;
			if (general != null)
				msgs = ((InternalEObject)general).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__GENERAL, null, msgs);
			if (newGeneral != null)
				msgs = ((InternalEObject)newGeneral).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__GENERAL, null, msgs);
			msgs = basicSetGeneral(newGeneral, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__GENERAL, newGeneral, newGeneral));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LoggingType getLogging() {
		return logging;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetLogging(LoggingType newLogging, NotificationChain msgs) {
		LoggingType oldLogging = logging;
		logging = newLogging;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__LOGGING, oldLogging, newLogging);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLogging(LoggingType newLogging) {
		if (newLogging != logging) {
			NotificationChain msgs = null;
			if (logging != null)
				msgs = ((InternalEObject)logging).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__LOGGING, null, msgs);
			if (newLogging != null)
				msgs = ((InternalEObject)newLogging).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__LOGGING, null, msgs);
			msgs = basicSetLogging(newLogging, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__LOGGING, newLogging, newLogging));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelInferenceType getModelInference() {
		return modelInference;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetModelInference(ModelInferenceType newModelInference, NotificationChain msgs) {
		ModelInferenceType oldModelInference = modelInference;
		modelInference = newModelInference;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE, oldModelInference, newModelInference);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setModelInference(ModelInferenceType newModelInference) {
		if (newModelInference != modelInference) {
			NotificationChain msgs = null;
			if (modelInference != null)
				msgs = ((InternalEObject)modelInference).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE, null, msgs);
			if (newModelInference != null)
				msgs = ((InternalEObject)newModelInference).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE, null, msgs);
			msgs = basicSetModelInference(newModelInference, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE, newModelInference, newModelInference));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TestGenerationType getTestGeneration() {
		return testGeneration;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetTestGeneration(TestGenerationType newTestGeneration, NotificationChain msgs) {
		TestGenerationType oldTestGeneration = testGeneration;
		testGeneration = newTestGeneration;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION, oldTestGeneration, newTestGeneration);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTestGeneration(TestGenerationType newTestGeneration) {
		if (newTestGeneration != testGeneration) {
			NotificationChain msgs = null;
			if (testGeneration != null)
				msgs = ((InternalEObject)testGeneration).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION, null, msgs);
			if (newTestGeneration != null)
				msgs = ((InternalEObject)newTestGeneration).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION, null, msgs);
			msgs = basicSetTestGeneration(newTestGeneration, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION, newTestGeneration, newTestGeneration));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OracleType getOracle() {
		return oracle;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetOracle(OracleType newOracle, NotificationChain msgs) {
		OracleType oldOracle = oracle;
		oracle = newOracle;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__ORACLE, oldOracle, newOracle);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setOracle(OracleType newOracle) {
		if (newOracle != oracle) {
			NotificationChain msgs = null;
			if (oracle != null)
				msgs = ((InternalEObject)oracle).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__ORACLE, null, msgs);
			if (newOracle != null)
				msgs = ((InternalEObject)newOracle).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.TEST_PROJECT_TYPE__ORACLE, null, msgs);
			msgs = basicSetOracle(newOracle, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.TEST_PROJECT_TYPE__ORACLE, newOracle, newOracle));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ProjectPackage.TEST_PROJECT_TYPE__GENERAL:
				return basicSetGeneral(null, msgs);
			case ProjectPackage.TEST_PROJECT_TYPE__LOGGING:
				return basicSetLogging(null, msgs);
			case ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE:
				return basicSetModelInference(null, msgs);
			case ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION:
				return basicSetTestGeneration(null, msgs);
			case ProjectPackage.TEST_PROJECT_TYPE__ORACLE:
				return basicSetOracle(null, msgs);
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
			case ProjectPackage.TEST_PROJECT_TYPE__GENERAL:
				return getGeneral();
			case ProjectPackage.TEST_PROJECT_TYPE__LOGGING:
				return getLogging();
			case ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE:
				return getModelInference();
			case ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION:
				return getTestGeneration();
			case ProjectPackage.TEST_PROJECT_TYPE__ORACLE:
				return getOracle();
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
			case ProjectPackage.TEST_PROJECT_TYPE__GENERAL:
				setGeneral((GeneralType)newValue);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__LOGGING:
				setLogging((LoggingType)newValue);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE:
				setModelInference((ModelInferenceType)newValue);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION:
				setTestGeneration((TestGenerationType)newValue);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__ORACLE:
				setOracle((OracleType)newValue);
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
			case ProjectPackage.TEST_PROJECT_TYPE__GENERAL:
				setGeneral((GeneralType)null);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__LOGGING:
				setLogging((LoggingType)null);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE:
				setModelInference((ModelInferenceType)null);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION:
				setTestGeneration((TestGenerationType)null);
				return;
			case ProjectPackage.TEST_PROJECT_TYPE__ORACLE:
				setOracle((OracleType)null);
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
			case ProjectPackage.TEST_PROJECT_TYPE__GENERAL:
				return general != null;
			case ProjectPackage.TEST_PROJECT_TYPE__LOGGING:
				return logging != null;
			case ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE:
				return modelInference != null;
			case ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION:
				return testGeneration != null;
			case ProjectPackage.TEST_PROJECT_TYPE__ORACLE:
				return oracle != null;
		}
		return super.eIsSet(featureID);
	}

} //TestProjectTypeImpl
