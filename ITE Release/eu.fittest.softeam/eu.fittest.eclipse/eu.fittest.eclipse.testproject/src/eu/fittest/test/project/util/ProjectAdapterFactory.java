/**
 */
package eu.fittest.test.project.util;

import eu.fittest.test.project.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage
 * @generated
 */
public class ProjectAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static ProjectPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ProjectAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = ProjectPackage.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
	 * <!-- end-user-doc -->
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage) {
			return true;
		}
		if (object instanceof EObject) {
			return ((EObject)object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ProjectSwitch modelSwitch =
		new ProjectSwitch() {
			public Object caseDocumentRoot(DocumentRoot object) {
				return createDocumentRootAdapter();
			}
			public Object caseGAParameterType(GAParameterType object) {
				return createGAParameterTypeAdapter();
			}
			public Object caseGeneralType(GeneralType object) {
				return createGeneralTypeAdapter();
			}
			public Object caseInstrumentationType(InstrumentationType object) {
				return createInstrumentationTypeAdapter();
			}
			public Object caseLoggingType(LoggingType object) {
				return createLoggingTypeAdapter();
			}
			public Object caseLogTargetType(LogTargetType object) {
				return createLogTargetTypeAdapter();
			}
			public Object caseModelInferenceType(ModelInferenceType object) {
				return createModelInferenceTypeAdapter();
			}
			public Object caseOracleType(OracleType object) {
				return createOracleTypeAdapter();
			}
			public Object caseTestGenerationType(TestGenerationType object) {
				return createTestGenerationTypeAdapter();
			}
			public Object caseTestProjectType(TestProjectType object) {
				return createTestProjectTypeAdapter();
			}
			public Object defaultCase(EObject object) {
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	public Adapter createAdapter(Notifier target) {
		return (Adapter)modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.DocumentRoot <em>Document Root</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.DocumentRoot
	 * @generated
	 */
	public Adapter createDocumentRootAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.GAParameterType <em>GA Parameter Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.GAParameterType
	 * @generated
	 */
	public Adapter createGAParameterTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.GeneralType <em>General Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.GeneralType
	 * @generated
	 */
	public Adapter createGeneralTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.InstrumentationType <em>Instrumentation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.InstrumentationType
	 * @generated
	 */
	public Adapter createInstrumentationTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.LoggingType <em>Logging Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.LoggingType
	 * @generated
	 */
	public Adapter createLoggingTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.LogTargetType <em>Log Target Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.LogTargetType
	 * @generated
	 */
	public Adapter createLogTargetTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.ModelInferenceType <em>Model Inference Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.ModelInferenceType
	 * @generated
	 */
	public Adapter createModelInferenceTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.OracleType <em>Oracle Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.OracleType
	 * @generated
	 */
	public Adapter createOracleTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.TestGenerationType <em>Test Generation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.TestGenerationType
	 * @generated
	 */
	public Adapter createTestGenerationTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link eu.fittest.test.project.TestProjectType <em>Test Project Type</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see eu.fittest.test.project.TestProjectType
	 * @generated
	 */
	public Adapter createTestProjectTypeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

} //ProjectAdapterFactory
