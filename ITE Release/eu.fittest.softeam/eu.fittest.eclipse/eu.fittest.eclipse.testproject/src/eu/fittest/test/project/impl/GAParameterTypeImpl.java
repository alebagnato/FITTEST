/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.GAParameterType;
import eu.fittest.test.project.ProjectPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>GA Parameter Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.GAParameterTypeImpl#getPopulationSize <em>Population Size</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GAParameterTypeImpl#getChromosomeLength <em>Chromosome Length</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GAParameterTypeImpl#getMaxNumberOfGenerations <em>Max Number Of Generations</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GAParameterTypeImpl#getMutationRate <em>Mutation Rate</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GAParameterTypeImpl#getTimeBudget <em>Time Budget</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GAParameterTypeImpl#getStopPort <em>Stop Port</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class GAParameterTypeImpl extends EObjectImpl implements GAParameterType {
	/**
	 * The default value of the '{@link #getPopulationSize() <em>Population Size</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPopulationSize()
	 * @generated
	 * @ordered
	 */
	protected static final int POPULATION_SIZE_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getPopulationSize() <em>Population Size</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPopulationSize()
	 * @generated
	 * @ordered
	 */
	protected int populationSize = POPULATION_SIZE_EDEFAULT;

	/**
	 * This is true if the Population Size attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean populationSizeESet;

	/**
	 * The default value of the '{@link #getChromosomeLength() <em>Chromosome Length</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChromosomeLength()
	 * @generated
	 * @ordered
	 */
	protected static final int CHROMOSOME_LENGTH_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getChromosomeLength() <em>Chromosome Length</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChromosomeLength()
	 * @generated
	 * @ordered
	 */
	protected int chromosomeLength = CHROMOSOME_LENGTH_EDEFAULT;

	/**
	 * This is true if the Chromosome Length attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean chromosomeLengthESet;

	/**
	 * The default value of the '{@link #getMaxNumberOfGenerations() <em>Max Number Of Generations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMaxNumberOfGenerations()
	 * @generated
	 * @ordered
	 */
	protected static final int MAX_NUMBER_OF_GENERATIONS_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getMaxNumberOfGenerations() <em>Max Number Of Generations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMaxNumberOfGenerations()
	 * @generated
	 * @ordered
	 */
	protected int maxNumberOfGenerations = MAX_NUMBER_OF_GENERATIONS_EDEFAULT;

	/**
	 * This is true if the Max Number Of Generations attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean maxNumberOfGenerationsESet;

	/**
	 * The default value of the '{@link #getMutationRate() <em>Mutation Rate</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMutationRate()
	 * @generated
	 * @ordered
	 */
	protected static final double MUTATION_RATE_EDEFAULT = 0.0;

	/**
	 * The cached value of the '{@link #getMutationRate() <em>Mutation Rate</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMutationRate()
	 * @generated
	 * @ordered
	 */
	protected double mutationRate = MUTATION_RATE_EDEFAULT;

	/**
	 * This is true if the Mutation Rate attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean mutationRateESet;

	/**
	 * The default value of the '{@link #getTimeBudget() <em>Time Budget</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTimeBudget()
	 * @generated
	 * @ordered
	 */
	protected static final int TIME_BUDGET_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getTimeBudget() <em>Time Budget</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTimeBudget()
	 * @generated
	 * @ordered
	 */
	protected int timeBudget = TIME_BUDGET_EDEFAULT;

	/**
	 * This is true if the Time Budget attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean timeBudgetESet;

	/**
	 * The default value of the '{@link #getStopPort() <em>Stop Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStopPort()
	 * @generated
	 * @ordered
	 */
	protected static final int STOP_PORT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getStopPort() <em>Stop Port</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStopPort()
	 * @generated
	 * @ordered
	 */
	protected int stopPort = STOP_PORT_EDEFAULT;

	/**
	 * This is true if the Stop Port attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean stopPortESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected GAParameterTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.GA_PARAMETER_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getPopulationSize() {
		return populationSize;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPopulationSize(int newPopulationSize) {
		int oldPopulationSize = populationSize;
		populationSize = newPopulationSize;
		boolean oldPopulationSizeESet = populationSizeESet;
		populationSizeESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GA_PARAMETER_TYPE__POPULATION_SIZE, oldPopulationSize, populationSize, !oldPopulationSizeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetPopulationSize() {
		int oldPopulationSize = populationSize;
		boolean oldPopulationSizeESet = populationSizeESet;
		populationSize = POPULATION_SIZE_EDEFAULT;
		populationSizeESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.GA_PARAMETER_TYPE__POPULATION_SIZE, oldPopulationSize, POPULATION_SIZE_EDEFAULT, oldPopulationSizeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetPopulationSize() {
		return populationSizeESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getChromosomeLength() {
		return chromosomeLength;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setChromosomeLength(int newChromosomeLength) {
		int oldChromosomeLength = chromosomeLength;
		chromosomeLength = newChromosomeLength;
		boolean oldChromosomeLengthESet = chromosomeLengthESet;
		chromosomeLengthESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH, oldChromosomeLength, chromosomeLength, !oldChromosomeLengthESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetChromosomeLength() {
		int oldChromosomeLength = chromosomeLength;
		boolean oldChromosomeLengthESet = chromosomeLengthESet;
		chromosomeLength = CHROMOSOME_LENGTH_EDEFAULT;
		chromosomeLengthESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH, oldChromosomeLength, CHROMOSOME_LENGTH_EDEFAULT, oldChromosomeLengthESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetChromosomeLength() {
		return chromosomeLengthESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getMaxNumberOfGenerations() {
		return maxNumberOfGenerations;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMaxNumberOfGenerations(int newMaxNumberOfGenerations) {
		int oldMaxNumberOfGenerations = maxNumberOfGenerations;
		maxNumberOfGenerations = newMaxNumberOfGenerations;
		boolean oldMaxNumberOfGenerationsESet = maxNumberOfGenerationsESet;
		maxNumberOfGenerationsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS, oldMaxNumberOfGenerations, maxNumberOfGenerations, !oldMaxNumberOfGenerationsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetMaxNumberOfGenerations() {
		int oldMaxNumberOfGenerations = maxNumberOfGenerations;
		boolean oldMaxNumberOfGenerationsESet = maxNumberOfGenerationsESet;
		maxNumberOfGenerations = MAX_NUMBER_OF_GENERATIONS_EDEFAULT;
		maxNumberOfGenerationsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS, oldMaxNumberOfGenerations, MAX_NUMBER_OF_GENERATIONS_EDEFAULT, oldMaxNumberOfGenerationsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetMaxNumberOfGenerations() {
		return maxNumberOfGenerationsESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public double getMutationRate() {
		return mutationRate;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMutationRate(double newMutationRate) {
		double oldMutationRate = mutationRate;
		mutationRate = newMutationRate;
		boolean oldMutationRateESet = mutationRateESet;
		mutationRateESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GA_PARAMETER_TYPE__MUTATION_RATE, oldMutationRate, mutationRate, !oldMutationRateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetMutationRate() {
		double oldMutationRate = mutationRate;
		boolean oldMutationRateESet = mutationRateESet;
		mutationRate = MUTATION_RATE_EDEFAULT;
		mutationRateESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.GA_PARAMETER_TYPE__MUTATION_RATE, oldMutationRate, MUTATION_RATE_EDEFAULT, oldMutationRateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetMutationRate() {
		return mutationRateESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getTimeBudget() {
		return timeBudget;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTimeBudget(int newTimeBudget) {
		int oldTimeBudget = timeBudget;
		timeBudget = newTimeBudget;
		boolean oldTimeBudgetESet = timeBudgetESet;
		timeBudgetESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GA_PARAMETER_TYPE__TIME_BUDGET, oldTimeBudget, timeBudget, !oldTimeBudgetESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetTimeBudget() {
		int oldTimeBudget = timeBudget;
		boolean oldTimeBudgetESet = timeBudgetESet;
		timeBudget = TIME_BUDGET_EDEFAULT;
		timeBudgetESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.GA_PARAMETER_TYPE__TIME_BUDGET, oldTimeBudget, TIME_BUDGET_EDEFAULT, oldTimeBudgetESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetTimeBudget() {
		return timeBudgetESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getStopPort() {
		return stopPort;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStopPort(int newStopPort) {
		int oldStopPort = stopPort;
		stopPort = newStopPort;
		boolean oldStopPortESet = stopPortESet;
		stopPortESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GA_PARAMETER_TYPE__STOP_PORT, oldStopPort, stopPort, !oldStopPortESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetStopPort() {
		int oldStopPort = stopPort;
		boolean oldStopPortESet = stopPortESet;
		stopPort = STOP_PORT_EDEFAULT;
		stopPortESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.GA_PARAMETER_TYPE__STOP_PORT, oldStopPort, STOP_PORT_EDEFAULT, oldStopPortESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetStopPort() {
		return stopPortESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.GA_PARAMETER_TYPE__POPULATION_SIZE:
				return new Integer(getPopulationSize());
			case ProjectPackage.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH:
				return new Integer(getChromosomeLength());
			case ProjectPackage.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS:
				return new Integer(getMaxNumberOfGenerations());
			case ProjectPackage.GA_PARAMETER_TYPE__MUTATION_RATE:
				return new Double(getMutationRate());
			case ProjectPackage.GA_PARAMETER_TYPE__TIME_BUDGET:
				return new Integer(getTimeBudget());
			case ProjectPackage.GA_PARAMETER_TYPE__STOP_PORT:
				return new Integer(getStopPort());
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
			case ProjectPackage.GA_PARAMETER_TYPE__POPULATION_SIZE:
				setPopulationSize(((Integer)newValue).intValue());
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH:
				setChromosomeLength(((Integer)newValue).intValue());
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS:
				setMaxNumberOfGenerations(((Integer)newValue).intValue());
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__MUTATION_RATE:
				setMutationRate(((Double)newValue).doubleValue());
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__TIME_BUDGET:
				setTimeBudget(((Integer)newValue).intValue());
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__STOP_PORT:
				setStopPort(((Integer)newValue).intValue());
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
			case ProjectPackage.GA_PARAMETER_TYPE__POPULATION_SIZE:
				unsetPopulationSize();
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH:
				unsetChromosomeLength();
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS:
				unsetMaxNumberOfGenerations();
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__MUTATION_RATE:
				unsetMutationRate();
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__TIME_BUDGET:
				unsetTimeBudget();
				return;
			case ProjectPackage.GA_PARAMETER_TYPE__STOP_PORT:
				unsetStopPort();
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
			case ProjectPackage.GA_PARAMETER_TYPE__POPULATION_SIZE:
				return isSetPopulationSize();
			case ProjectPackage.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH:
				return isSetChromosomeLength();
			case ProjectPackage.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS:
				return isSetMaxNumberOfGenerations();
			case ProjectPackage.GA_PARAMETER_TYPE__MUTATION_RATE:
				return isSetMutationRate();
			case ProjectPackage.GA_PARAMETER_TYPE__TIME_BUDGET:
				return isSetTimeBudget();
			case ProjectPackage.GA_PARAMETER_TYPE__STOP_PORT:
				return isSetStopPort();
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
		result.append(" (populationSize: ");
		if (populationSizeESet) result.append(populationSize); else result.append("<unset>");
		result.append(", chromosomeLength: ");
		if (chromosomeLengthESet) result.append(chromosomeLength); else result.append("<unset>");
		result.append(", maxNumberOfGenerations: ");
		if (maxNumberOfGenerationsESet) result.append(maxNumberOfGenerations); else result.append("<unset>");
		result.append(", mutationRate: ");
		if (mutationRateESet) result.append(mutationRate); else result.append("<unset>");
		result.append(", timeBudget: ");
		if (timeBudgetESet) result.append(timeBudget); else result.append("<unset>");
		result.append(", stopPort: ");
		if (stopPortESet) result.append(stopPort); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //GAParameterTypeImpl
