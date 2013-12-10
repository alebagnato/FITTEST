/**
 */
package eu.fittest.test.project.util;

import eu.fittest.test.project.*;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage
 * @generated
 */
public class ProjectSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static ProjectPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ProjectSwitch() {
		if (modelPackage == null) {
			modelPackage = ProjectPackage.eINSTANCE;
		}
	}

	/**
	 * Checks whether this is a switch for the given package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @parameter ePackage the package in question.
	 * @return whether this is a switch for the given package.
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/**
	 * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the first non-null result returned by a <code>caseXXX</code> call.
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case ProjectPackage.GA_PARAMETER_TYPE: {
				GAParameterType gaParameterType = (GAParameterType)theEObject;
				T result = caseGAParameterType(gaParameterType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.GENERAL_TYPE: {
				GeneralType generalType = (GeneralType)theEObject;
				T result = caseGeneralType(generalType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.INSTRUMENTATION_TYPE: {
				InstrumentationType instrumentationType = (InstrumentationType)theEObject;
				T result = caseInstrumentationType(instrumentationType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.LOGGING_TYPE: {
				LoggingType loggingType = (LoggingType)theEObject;
				T result = caseLoggingType(loggingType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.LOG_TARGET_TYPE: {
				LogTargetType logTargetType = (LogTargetType)theEObject;
				T result = caseLogTargetType(logTargetType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.MODEL_INFERENCE_TYPE: {
				ModelInferenceType modelInferenceType = (ModelInferenceType)theEObject;
				T result = caseModelInferenceType(modelInferenceType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.ORACLE_TYPE: {
				OracleType oracleType = (OracleType)theEObject;
				T result = caseOracleType(oracleType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.TEST_GENERATION_TYPE: {
				TestGenerationType testGenerationType = (TestGenerationType)theEObject;
				T result = caseTestGenerationType(testGenerationType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case ProjectPackage.TEST_PROJECT: {
				TestProject testProject = (TestProject)theEObject;
				T result = caseTestProject(testProject);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>GA Parameter Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>GA Parameter Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseGAParameterType(GAParameterType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>General Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>General Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseGeneralType(GeneralType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Instrumentation Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Instrumentation Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseInstrumentationType(InstrumentationType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Logging Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Logging Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLoggingType(LoggingType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Log Target Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Log Target Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLogTargetType(LogTargetType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Model Inference Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Model Inference Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseModelInferenceType(ModelInferenceType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Oracle Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Oracle Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseOracleType(OracleType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Test Generation Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Test Generation Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseTestGenerationType(TestGenerationType object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Test Project</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Test Project</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseTestProject(TestProject object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch, but this is the last case anyway.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

} //ProjectSwitch
