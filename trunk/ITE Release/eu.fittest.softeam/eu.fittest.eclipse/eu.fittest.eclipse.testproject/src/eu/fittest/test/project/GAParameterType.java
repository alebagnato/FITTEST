/**
 */
package eu.fittest.test.project;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>GA Parameter Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link eu.fittest.test.project.GAParameterType#getPopulationSize <em>Population Size</em>}</li>
 *   <li>{@link eu.fittest.test.project.GAParameterType#getChromosomeLength <em>Chromosome Length</em>}</li>
 *   <li>{@link eu.fittest.test.project.GAParameterType#getMaxNumberOfGenerations <em>Max Number Of Generations</em>}</li>
 *   <li>{@link eu.fittest.test.project.GAParameterType#getMutationRate <em>Mutation Rate</em>}</li>
 *   <li>{@link eu.fittest.test.project.GAParameterType#getTimeBudget <em>Time Budget</em>}</li>
 *   <li>{@link eu.fittest.test.project.GAParameterType#getStopPort <em>Stop Port</em>}</li>
 * </ul>
 * </p>
 *
 * @see eu.fittest.test.project.ProjectPackage#getGAParameterType()
 * @model extendedMetaData="name='GAParameterType' kind='elementOnly'"
 * @generated
 */
public interface GAParameterType extends EObject {
	/**
	 * Returns the value of the '<em><b>Population Size</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Population Size</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Population Size</em>' attribute.
	 * @see #isSetPopulationSize()
	 * @see #unsetPopulationSize()
	 * @see #setPopulationSize(int)
	 * @see eu.fittest.test.project.ProjectPackage#getGAParameterType_PopulationSize()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='populationSize' namespace='##targetNamespace'"
	 * @generated
	 */
	int getPopulationSize();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GAParameterType#getPopulationSize <em>Population Size</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Population Size</em>' attribute.
	 * @see #isSetPopulationSize()
	 * @see #unsetPopulationSize()
	 * @see #getPopulationSize()
	 * @generated
	 */
	void setPopulationSize(int value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.GAParameterType#getPopulationSize <em>Population Size</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetPopulationSize()
	 * @see #getPopulationSize()
	 * @see #setPopulationSize(int)
	 * @generated
	 */
	void unsetPopulationSize();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.GAParameterType#getPopulationSize <em>Population Size</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Population Size</em>' attribute is set.
	 * @see #unsetPopulationSize()
	 * @see #getPopulationSize()
	 * @see #setPopulationSize(int)
	 * @generated
	 */
	boolean isSetPopulationSize();

	/**
	 * Returns the value of the '<em><b>Chromosome Length</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Chromosome Length</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Chromosome Length</em>' attribute.
	 * @see #isSetChromosomeLength()
	 * @see #unsetChromosomeLength()
	 * @see #setChromosomeLength(int)
	 * @see eu.fittest.test.project.ProjectPackage#getGAParameterType_ChromosomeLength()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='chromosomeLength' namespace='##targetNamespace'"
	 * @generated
	 */
	int getChromosomeLength();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GAParameterType#getChromosomeLength <em>Chromosome Length</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Chromosome Length</em>' attribute.
	 * @see #isSetChromosomeLength()
	 * @see #unsetChromosomeLength()
	 * @see #getChromosomeLength()
	 * @generated
	 */
	void setChromosomeLength(int value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.GAParameterType#getChromosomeLength <em>Chromosome Length</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetChromosomeLength()
	 * @see #getChromosomeLength()
	 * @see #setChromosomeLength(int)
	 * @generated
	 */
	void unsetChromosomeLength();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.GAParameterType#getChromosomeLength <em>Chromosome Length</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Chromosome Length</em>' attribute is set.
	 * @see #unsetChromosomeLength()
	 * @see #getChromosomeLength()
	 * @see #setChromosomeLength(int)
	 * @generated
	 */
	boolean isSetChromosomeLength();

	/**
	 * Returns the value of the '<em><b>Max Number Of Generations</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Max Number Of Generations</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Max Number Of Generations</em>' attribute.
	 * @see #isSetMaxNumberOfGenerations()
	 * @see #unsetMaxNumberOfGenerations()
	 * @see #setMaxNumberOfGenerations(int)
	 * @see eu.fittest.test.project.ProjectPackage#getGAParameterType_MaxNumberOfGenerations()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='maxNumberOfGenerations' namespace='##targetNamespace'"
	 * @generated
	 */
	int getMaxNumberOfGenerations();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GAParameterType#getMaxNumberOfGenerations <em>Max Number Of Generations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Max Number Of Generations</em>' attribute.
	 * @see #isSetMaxNumberOfGenerations()
	 * @see #unsetMaxNumberOfGenerations()
	 * @see #getMaxNumberOfGenerations()
	 * @generated
	 */
	void setMaxNumberOfGenerations(int value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.GAParameterType#getMaxNumberOfGenerations <em>Max Number Of Generations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetMaxNumberOfGenerations()
	 * @see #getMaxNumberOfGenerations()
	 * @see #setMaxNumberOfGenerations(int)
	 * @generated
	 */
	void unsetMaxNumberOfGenerations();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.GAParameterType#getMaxNumberOfGenerations <em>Max Number Of Generations</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Max Number Of Generations</em>' attribute is set.
	 * @see #unsetMaxNumberOfGenerations()
	 * @see #getMaxNumberOfGenerations()
	 * @see #setMaxNumberOfGenerations(int)
	 * @generated
	 */
	boolean isSetMaxNumberOfGenerations();

	/**
	 * Returns the value of the '<em><b>Mutation Rate</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Mutation Rate</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Mutation Rate</em>' attribute.
	 * @see #isSetMutationRate()
	 * @see #unsetMutationRate()
	 * @see #setMutationRate(double)
	 * @see eu.fittest.test.project.ProjectPackage#getGAParameterType_MutationRate()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Double" required="true"
	 *        extendedMetaData="kind='element' name='mutationRate' namespace='##targetNamespace'"
	 * @generated
	 */
	double getMutationRate();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GAParameterType#getMutationRate <em>Mutation Rate</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Mutation Rate</em>' attribute.
	 * @see #isSetMutationRate()
	 * @see #unsetMutationRate()
	 * @see #getMutationRate()
	 * @generated
	 */
	void setMutationRate(double value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.GAParameterType#getMutationRate <em>Mutation Rate</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetMutationRate()
	 * @see #getMutationRate()
	 * @see #setMutationRate(double)
	 * @generated
	 */
	void unsetMutationRate();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.GAParameterType#getMutationRate <em>Mutation Rate</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Mutation Rate</em>' attribute is set.
	 * @see #unsetMutationRate()
	 * @see #getMutationRate()
	 * @see #setMutationRate(double)
	 * @generated
	 */
	boolean isSetMutationRate();

	/**
	 * Returns the value of the '<em><b>Time Budget</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Time in SECOND
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Time Budget</em>' attribute.
	 * @see #isSetTimeBudget()
	 * @see #unsetTimeBudget()
	 * @see #setTimeBudget(int)
	 * @see eu.fittest.test.project.ProjectPackage#getGAParameterType_TimeBudget()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='timeBudget' namespace='##targetNamespace'"
	 * @generated
	 */
	int getTimeBudget();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GAParameterType#getTimeBudget <em>Time Budget</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Time Budget</em>' attribute.
	 * @see #isSetTimeBudget()
	 * @see #unsetTimeBudget()
	 * @see #getTimeBudget()
	 * @generated
	 */
	void setTimeBudget(int value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.GAParameterType#getTimeBudget <em>Time Budget</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetTimeBudget()
	 * @see #getTimeBudget()
	 * @see #setTimeBudget(int)
	 * @generated
	 */
	void unsetTimeBudget();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.GAParameterType#getTimeBudget <em>Time Budget</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Time Budget</em>' attribute is set.
	 * @see #unsetTimeBudget()
	 * @see #getTimeBudget()
	 * @see #setTimeBudget(int)
	 * @generated
	 */
	boolean isSetTimeBudget();

	/**
	 * Returns the value of the '<em><b>Stop Port</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Stop Port</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Stop Port</em>' attribute.
	 * @see #isSetStopPort()
	 * @see #unsetStopPort()
	 * @see #setStopPort(int)
	 * @see eu.fittest.test.project.ProjectPackage#getGAParameterType_StopPort()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='stopPort' namespace='##targetNamespace'"
	 * @generated
	 */
	int getStopPort();

	/**
	 * Sets the value of the '{@link eu.fittest.test.project.GAParameterType#getStopPort <em>Stop Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Stop Port</em>' attribute.
	 * @see #isSetStopPort()
	 * @see #unsetStopPort()
	 * @see #getStopPort()
	 * @generated
	 */
	void setStopPort(int value);

	/**
	 * Unsets the value of the '{@link eu.fittest.test.project.GAParameterType#getStopPort <em>Stop Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetStopPort()
	 * @see #getStopPort()
	 * @see #setStopPort(int)
	 * @generated
	 */
	void unsetStopPort();

	/**
	 * Returns whether the value of the '{@link eu.fittest.test.project.GAParameterType#getStopPort <em>Stop Port</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Stop Port</em>' attribute is set.
	 * @see #unsetStopPort()
	 * @see #getStopPort()
	 * @see #setStopPort(int)
	 * @generated
	 */
	boolean isSetStopPort();

} // GAParameterType
