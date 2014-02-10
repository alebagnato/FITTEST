/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.GAParameterType;
import eu.fittest.test.project.InferenceTechniqueType;
import eu.fittest.test.project.ModelInferenceType;
import eu.fittest.test.project.ProjectPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Model Inference Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl#getDomainInputSpecFile <em>Domain Input Spec File</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl#getAbsFuncDefFile <em>Abs Func Def File</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl#getModelFile <em>Model File</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl#getInferenceTechnique <em>Inference Technique</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl#isGenerateDot <em>Generate Dot</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.ModelInferenceTypeImpl#getGaParam <em>Ga Param</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ModelInferenceTypeImpl extends EObjectImpl implements ModelInferenceType {
	/**
	 * The default value of the '{@link #getDomainInputSpecFile() <em>Domain Input Spec File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDomainInputSpecFile()
	 * @generated
	 * @ordered
	 */
	protected static final String DOMAIN_INPUT_SPEC_FILE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDomainInputSpecFile() <em>Domain Input Spec File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDomainInputSpecFile()
	 * @generated
	 * @ordered
	 */
	protected String domainInputSpecFile = DOMAIN_INPUT_SPEC_FILE_EDEFAULT;

	/**
	 * The default value of the '{@link #getAbsFuncDefFile() <em>Abs Func Def File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAbsFuncDefFile()
	 * @generated
	 * @ordered
	 */
	protected static final String ABS_FUNC_DEF_FILE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getAbsFuncDefFile() <em>Abs Func Def File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAbsFuncDefFile()
	 * @generated
	 * @ordered
	 */
	protected String absFuncDefFile = ABS_FUNC_DEF_FILE_EDEFAULT;

	/**
	 * The default value of the '{@link #getModelFile() <em>Model File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelFile()
	 * @generated
	 * @ordered
	 */
	protected static final String MODEL_FILE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getModelFile() <em>Model File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getModelFile()
	 * @generated
	 * @ordered
	 */
	protected String modelFile = MODEL_FILE_EDEFAULT;

	/**
	 * The default value of the '{@link #getInferenceTechnique() <em>Inference Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInferenceTechnique()
	 * @generated
	 * @ordered
	 */
	protected static final InferenceTechniqueType INFERENCE_TECHNIQUE_EDEFAULT = InferenceTechniqueType.SEQUENCEBASED_LITERAL;

	/**
	 * The cached value of the '{@link #getInferenceTechnique() <em>Inference Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInferenceTechnique()
	 * @generated
	 * @ordered
	 */
	protected InferenceTechniqueType inferenceTechnique = INFERENCE_TECHNIQUE_EDEFAULT;

	/**
	 * This is true if the Inference Technique attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean inferenceTechniqueESet;

	/**
	 * The default value of the '{@link #isGenerateDot() <em>Generate Dot</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isGenerateDot()
	 * @generated
	 * @ordered
	 */
	protected static final boolean GENERATE_DOT_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isGenerateDot() <em>Generate Dot</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isGenerateDot()
	 * @generated
	 * @ordered
	 */
	protected boolean generateDot = GENERATE_DOT_EDEFAULT;

	/**
	 * This is true if the Generate Dot attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean generateDotESet;

	/**
	 * The cached value of the '{@link #getGaParam() <em>Ga Param</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGaParam()
	 * @generated
	 * @ordered
	 */
	protected GAParameterType gaParam;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ModelInferenceTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.MODEL_INFERENCE_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getDomainInputSpecFile() {
		return domainInputSpecFile;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setDomainInputSpecFile(String newDomainInputSpecFile) {
		String oldDomainInputSpecFile = domainInputSpecFile;
		domainInputSpecFile = newDomainInputSpecFile;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE, oldDomainInputSpecFile, domainInputSpecFile));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAbsFuncDefFile() {
		return absFuncDefFile;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAbsFuncDefFile(String newAbsFuncDefFile) {
		String oldAbsFuncDefFile = absFuncDefFile;
		absFuncDefFile = newAbsFuncDefFile;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE, oldAbsFuncDefFile, absFuncDefFile));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getModelFile() {
		return modelFile;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setModelFile(String newModelFile) {
		String oldModelFile = modelFile;
		modelFile = newModelFile;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.MODEL_INFERENCE_TYPE__MODEL_FILE, oldModelFile, modelFile));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InferenceTechniqueType getInferenceTechnique() {
		return inferenceTechnique;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setInferenceTechnique(InferenceTechniqueType newInferenceTechnique) {
		InferenceTechniqueType oldInferenceTechnique = inferenceTechnique;
		inferenceTechnique = newInferenceTechnique == null ? INFERENCE_TECHNIQUE_EDEFAULT : newInferenceTechnique;
		boolean oldInferenceTechniqueESet = inferenceTechniqueESet;
		inferenceTechniqueESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE, oldInferenceTechnique, inferenceTechnique, !oldInferenceTechniqueESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetInferenceTechnique() {
		InferenceTechniqueType oldInferenceTechnique = inferenceTechnique;
		boolean oldInferenceTechniqueESet = inferenceTechniqueESet;
		inferenceTechnique = INFERENCE_TECHNIQUE_EDEFAULT;
		inferenceTechniqueESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE, oldInferenceTechnique, INFERENCE_TECHNIQUE_EDEFAULT, oldInferenceTechniqueESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetInferenceTechnique() {
		return inferenceTechniqueESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isGenerateDot() {
		return generateDot;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGenerateDot(boolean newGenerateDot) {
		boolean oldGenerateDot = generateDot;
		generateDot = newGenerateDot;
		boolean oldGenerateDotESet = generateDotESet;
		generateDotESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.MODEL_INFERENCE_TYPE__GENERATE_DOT, oldGenerateDot, generateDot, !oldGenerateDotESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetGenerateDot() {
		boolean oldGenerateDot = generateDot;
		boolean oldGenerateDotESet = generateDotESet;
		generateDot = GENERATE_DOT_EDEFAULT;
		generateDotESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.MODEL_INFERENCE_TYPE__GENERATE_DOT, oldGenerateDot, GENERATE_DOT_EDEFAULT, oldGenerateDotESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetGenerateDot() {
		return generateDotESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GAParameterType getGaParam() {
		return gaParam;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetGaParam(GAParameterType newGaParam, NotificationChain msgs) {
		GAParameterType oldGaParam = gaParam;
		gaParam = newGaParam;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM, oldGaParam, newGaParam);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGaParam(GAParameterType newGaParam) {
		if (newGaParam != gaParam) {
			NotificationChain msgs = null;
			if (gaParam != null)
				msgs = ((InternalEObject)gaParam).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM, null, msgs);
			if (newGaParam != null)
				msgs = ((InternalEObject)newGaParam).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM, null, msgs);
			msgs = basicSetGaParam(newGaParam, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM, newGaParam, newGaParam));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM:
				return basicSetGaParam(null, msgs);
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
			case ProjectPackage.MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE:
				return getDomainInputSpecFile();
			case ProjectPackage.MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE:
				return getAbsFuncDefFile();
			case ProjectPackage.MODEL_INFERENCE_TYPE__MODEL_FILE:
				return getModelFile();
			case ProjectPackage.MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE:
				return getInferenceTechnique();
			case ProjectPackage.MODEL_INFERENCE_TYPE__GENERATE_DOT:
				return isGenerateDot() ? Boolean.TRUE : Boolean.FALSE;
			case ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM:
				return getGaParam();
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
			case ProjectPackage.MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE:
				setDomainInputSpecFile((String)newValue);
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE:
				setAbsFuncDefFile((String)newValue);
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__MODEL_FILE:
				setModelFile((String)newValue);
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE:
				setInferenceTechnique((InferenceTechniqueType)newValue);
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__GENERATE_DOT:
				setGenerateDot(((Boolean)newValue).booleanValue());
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM:
				setGaParam((GAParameterType)newValue);
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
			case ProjectPackage.MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE:
				setDomainInputSpecFile(DOMAIN_INPUT_SPEC_FILE_EDEFAULT);
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE:
				setAbsFuncDefFile(ABS_FUNC_DEF_FILE_EDEFAULT);
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__MODEL_FILE:
				setModelFile(MODEL_FILE_EDEFAULT);
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE:
				unsetInferenceTechnique();
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__GENERATE_DOT:
				unsetGenerateDot();
				return;
			case ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM:
				setGaParam((GAParameterType)null);
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
			case ProjectPackage.MODEL_INFERENCE_TYPE__DOMAIN_INPUT_SPEC_FILE:
				return DOMAIN_INPUT_SPEC_FILE_EDEFAULT == null ? domainInputSpecFile != null : !DOMAIN_INPUT_SPEC_FILE_EDEFAULT.equals(domainInputSpecFile);
			case ProjectPackage.MODEL_INFERENCE_TYPE__ABS_FUNC_DEF_FILE:
				return ABS_FUNC_DEF_FILE_EDEFAULT == null ? absFuncDefFile != null : !ABS_FUNC_DEF_FILE_EDEFAULT.equals(absFuncDefFile);
			case ProjectPackage.MODEL_INFERENCE_TYPE__MODEL_FILE:
				return MODEL_FILE_EDEFAULT == null ? modelFile != null : !MODEL_FILE_EDEFAULT.equals(modelFile);
			case ProjectPackage.MODEL_INFERENCE_TYPE__INFERENCE_TECHNIQUE:
				return isSetInferenceTechnique();
			case ProjectPackage.MODEL_INFERENCE_TYPE__GENERATE_DOT:
				return isSetGenerateDot();
			case ProjectPackage.MODEL_INFERENCE_TYPE__GA_PARAM:
				return gaParam != null;
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
		result.append(" (domainInputSpecFile: ");
		result.append(domainInputSpecFile);
		result.append(", absFuncDefFile: ");
		result.append(absFuncDefFile);
		result.append(", modelFile: ");
		result.append(modelFile);
		result.append(", inferenceTechnique: ");
		if (inferenceTechniqueESet) result.append(inferenceTechnique); else result.append("<unset>");
		result.append(", generateDot: ");
		if (generateDotESet) result.append(generateDot); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //ModelInferenceTypeImpl
