/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model Inference Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.ModelInferenceType#getDomainInputSpecFile <em>Domain Input Spec File</em>}</li>
 *   <li>{@link eu.fittest.test.project.ModelInferenceType#getAbsFuncDefFile <em>Abs Func Def File</em>}</li>
 *   <li>{@link eu.fittest.test.project.ModelInferenceType#getModelFile <em>Model File</em>}</li>
 *   <li>{@link eu.fittest.test.project.ModelInferenceType#getInferenceTechnique <em>Inference Technique</em>}</li>
 *   <li>{@link eu.fittest.test.project.ModelInferenceType#isGenerateDot <em>Generate Dot</em>}</li>
 *   <li>{@link eu.fittest.test.project.ModelInferenceType#getGaParam <em>Ga Param</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getModelInferenceType()
 * @model extendedMetaData="name='ModelInferenceType' kind='elementOnly'"
 * @generated
 */
public interface ModelInferenceType extends EObject {
	/**
	 * Returns the value of the '<em><b>Domain Input Spec File</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Path to a domain input specification file
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Domain Input Spec File</em>' attribute.
	 * @see #setDomainInputSpecFile(String)
	 * @see eu.fittest.test.project.ProjectPackage#getModelInferenceType_DomainInputSpecFile()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='domainInputSpecFile' namespace='##targetNamespace'"
	 * @generated
	 */
	String getDomainInputSpecFile();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.ModelInferenceType#getDomainInputSpecFile <em>Domain Input Spec File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Domain Input Spec File</em>' attribute.
	 * @see #getDomainInputSpecFile()
	 * @generated
	 */
	void setDomainInputSpecFile(String value);

	/**
	 * Returns the value of the '<em><b>Abs Func Def File</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Path to a file containing the definitions of abstraction functions
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Abs Func Def File</em>' attribute.
	 * @see #setAbsFuncDefFile(String)
	 * @see eu.fittest.test.project.ProjectPackage#getModelInferenceType_AbsFuncDefFile()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='absFuncDefFile' namespace='##targetNamespace'"
	 * @generated
	 */
	String getAbsFuncDefFile();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.ModelInferenceType#getAbsFuncDefFile <em>Abs Func Def File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Abs Func Def File</em>' attribute.
	 * @see #getAbsFuncDefFile()
	 * @generated
	 */
	void setAbsFuncDefFile(String value);

	/**
	 * Returns the value of the '<em><b>Model File</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Model File</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Model File</em>' attribute.
	 * @see #setModelFile(String)
	 * @see eu.fittest.test.project.ProjectPackage#getModelInferenceType_ModelFile()
	 * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
	 *        extendedMetaData="kind='element' name='modelFile' namespace='##targetNamespace'"
	 * @generated
	 */
	String getModelFile();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.ModelInferenceType#getModelFile <em>Model File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Model File</em>' attribute.
	 * @see #getModelFile()
	 * @generated
	 */
	void setModelFile(String value);

	/**
	 * Returns the value of the '<em><b>Inference Technique</b></em>' attribute.
	 * The literals are from the enumeration {@link eu.fittest.test.project.InferenceTechniqueType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Inference Technique</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Inference Technique</em>' attribute.
	 * @see eu.fittest.test.project.InferenceTechniqueType
	 * @see #isSetInferenceTechnique()
	 * @see #unsetInferenceTechnique()
	 * @see #setInferenceTechnique(InferenceTechniqueType)
	 * @see eu.fittest.test.project.ProjectPackage#getModelInferenceType_InferenceTechnique()
	 * @model unsettable="true" required="true"
	 *        extendedMetaData="kind='element' name='inferenceTechnique' namespace='##targetNamespace'"
	 * @generated
	 */
	InferenceTechniqueType getInferenceTechnique();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.ModelInferenceType#getInferenceTechnique <em>Inference Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Inference Technique</em>' attribute.
	 * @see eu.fittest.test.project.InferenceTechniqueType
	 * @see #isSetInferenceTechnique()
	 * @see #unsetInferenceTechnique()
	 * @see #getInferenceTechnique()
	 * @generated
	 */
	void setInferenceTechnique(InferenceTechniqueType value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.ModelInferenceType#getInferenceTechnique <em>Inference Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetInferenceTechnique()
	 * @see #getInferenceTechnique()
	 * @see #setInferenceTechnique(InferenceTechniqueType)
	 * @generated
	 */
	void unsetInferenceTechnique();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.ModelInferenceType#getInferenceTechnique <em>Inference Technique</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Inference Technique</em>' attribute is set.
	 * @see #unsetInferenceTechnique()
	 * @see #getInferenceTechnique()
	 * @see #setInferenceTechnique(InferenceTechniqueType)
	 * @generated
	 */
	boolean isSetInferenceTechnique();

	/**
	 * Returns the value of the '<em><b>Generate Dot</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Generate Dot</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Generate Dot</em>' attribute.
	 * @see #isSetGenerateDot()
	 * @see #unsetGenerateDot()
	 * @see #setGenerateDot(boolean)
	 * @see eu.fittest.test.project.ProjectPackage#getModelInferenceType_GenerateDot()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Boolean" required="true"
	 *        extendedMetaData="kind='element' name='generateDot' namespace='##targetNamespace'"
	 * @generated
	 */
	boolean isGenerateDot();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.ModelInferenceType#isGenerateDot <em>Generate Dot</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Generate Dot</em>' attribute.
	 * @see #isSetGenerateDot()
	 * @see #unsetGenerateDot()
	 * @see #isGenerateDot()
	 * @generated
	 */
	void setGenerateDot(boolean value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.ModelInferenceType#isGenerateDot <em>Generate Dot</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetGenerateDot()
	 * @see #isGenerateDot()
	 * @see #setGenerateDot(boolean)
	 * @generated
	 */
	void unsetGenerateDot();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.ModelInferenceType#isGenerateDot <em>Generate Dot</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Generate Dot</em>' attribute is set.
	 * @see #unsetGenerateDot()
	 * @see #isGenerateDot()
	 * @see #setGenerateDot(boolean)
	 * @generated
	 */
	boolean isSetGenerateDot();

	/**
	 * Returns the value of the '<em><b>Ga Param</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ga Param</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ga Param</em>' containment reference.
	 * @see #setGaParam(GAParameterType)
	 * @see eu.fittest.test.project.ProjectPackage#getModelInferenceType_GaParam()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='gaParam' namespace='##targetNamespace'"
	 * @generated
	 */
	GAParameterType getGaParam();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.ModelInferenceType#getGaParam <em>Ga Param</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ga Param</em>' containment reference.
	 * @see #getGaParam()
	 * @generated
	 */
	void setGaParam(GAParameterType value);

} // ModelInferenceType
