/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.OracleType;
import eu.fittest.test.project.ProjectPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Oracle Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getGHCRTopts <em>GHCR Topts</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getOracleFile <em>Oracle File</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getReportFile <em>Report File</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getEventsToInclude <em>Events To Include</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getFieldsToInclude <em>Fields To Include</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getFunctionsToInclude <em>Functions To Include</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getLloOption <em>Llo Option</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.OracleTypeImpl#getViolationFile <em>Violation File</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class OracleTypeImpl extends EObjectImpl implements OracleType {
	/**
	 * The default value of the '{@link #getGHCRTopts() <em>GHCR Topts</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGHCRTopts()
	 * @generated
	 * @ordered
	 */
	protected static final String GHCR_TOPTS_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getGHCRTopts() <em>GHCR Topts</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGHCRTopts()
	 * @generated
	 * @ordered
	 */
	protected String gHCRTopts = GHCR_TOPTS_EDEFAULT;

	/**
	 * The default value of the '{@link #getOracleFile() <em>Oracle File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOracleFile()
	 * @generated
	 * @ordered
	 */
	protected static final String ORACLE_FILE_EDEFAULT = "oracle.inv";

	/**
	 * The cached value of the '{@link #getOracleFile() <em>Oracle File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOracleFile()
	 * @generated
	 * @ordered
	 */
	protected String oracleFile = ORACLE_FILE_EDEFAULT;

	/**
	 * This is true if the Oracle File attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean oracleFileESet;

	/**
	 * The default value of the '{@link #getReportFile() <em>Report File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReportFile()
	 * @generated
	 * @ordered
	 */
	protected static final String REPORT_FILE_EDEFAULT = "report.txt";

	/**
	 * The cached value of the '{@link #getReportFile() <em>Report File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReportFile()
	 * @generated
	 * @ordered
	 */
	protected String reportFile = REPORT_FILE_EDEFAULT;

	/**
	 * This is true if the Report File attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean reportFileESet;

	/**
	 * The default value of the '{@link #getEventsToInclude() <em>Events To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEventsToInclude()
	 * @generated
	 * @ordered
	 */
	protected static final String EVENTS_TO_INCLUDE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEventsToInclude() <em>Events To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEventsToInclude()
	 * @generated
	 * @ordered
	 */
	protected String eventsToInclude = EVENTS_TO_INCLUDE_EDEFAULT;

	/**
	 * The default value of the '{@link #getFieldsToInclude() <em>Fields To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFieldsToInclude()
	 * @generated
	 * @ordered
	 */
	protected static final String FIELDS_TO_INCLUDE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFieldsToInclude() <em>Fields To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFieldsToInclude()
	 * @generated
	 * @ordered
	 */
	protected String fieldsToInclude = FIELDS_TO_INCLUDE_EDEFAULT;

	/**
	 * The default value of the '{@link #getFunctionsToInclude() <em>Functions To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFunctionsToInclude()
	 * @generated
	 * @ordered
	 */
	protected static final String FUNCTIONS_TO_INCLUDE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFunctionsToInclude() <em>Functions To Include</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFunctionsToInclude()
	 * @generated
	 * @ordered
	 */
	protected String functionsToInclude = FUNCTIONS_TO_INCLUDE_EDEFAULT;

	/**
	 * The default value of the '{@link #getLloOption() <em>Llo Option</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLloOption()
	 * @generated
	 * @ordered
	 */
	protected static final String LLO_OPTION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLloOption() <em>Llo Option</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLloOption()
	 * @generated
	 * @ordered
	 */
	protected String lloOption = LLO_OPTION_EDEFAULT;

	/**
	 * The default value of the '{@link #getViolationFile() <em>Violation File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getViolationFile()
	 * @generated
	 * @ordered
	 */
	protected static final String VIOLATION_FILE_EDEFAULT = "violations.txt";

	/**
	 * The cached value of the '{@link #getViolationFile() <em>Violation File</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getViolationFile()
	 * @generated
	 * @ordered
	 */
	protected String violationFile = VIOLATION_FILE_EDEFAULT;

	/**
	 * This is true if the Violation File attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean violationFileESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected OracleTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.ORACLE_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getGHCRTopts() {
		return gHCRTopts;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGHCRTopts(String newGHCRTopts) {
		String oldGHCRTopts = gHCRTopts;
		gHCRTopts = newGHCRTopts;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__GHCR_TOPTS, oldGHCRTopts, gHCRTopts));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getOracleFile() {
		return oracleFile;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setOracleFile(String newOracleFile) {
		String oldOracleFile = oracleFile;
		oracleFile = newOracleFile;
		boolean oldOracleFileESet = oracleFileESet;
		oracleFileESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__ORACLE_FILE, oldOracleFile, oracleFile, !oldOracleFileESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetOracleFile() {
		String oldOracleFile = oracleFile;
		boolean oldOracleFileESet = oracleFileESet;
		oracleFile = ORACLE_FILE_EDEFAULT;
		oracleFileESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.ORACLE_TYPE__ORACLE_FILE, oldOracleFile, ORACLE_FILE_EDEFAULT, oldOracleFileESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetOracleFile() {
		return oracleFileESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getReportFile() {
		return reportFile;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setReportFile(String newReportFile) {
		String oldReportFile = reportFile;
		reportFile = newReportFile;
		boolean oldReportFileESet = reportFileESet;
		reportFileESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__REPORT_FILE, oldReportFile, reportFile, !oldReportFileESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetReportFile() {
		String oldReportFile = reportFile;
		boolean oldReportFileESet = reportFileESet;
		reportFile = REPORT_FILE_EDEFAULT;
		reportFileESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.ORACLE_TYPE__REPORT_FILE, oldReportFile, REPORT_FILE_EDEFAULT, oldReportFileESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetReportFile() {
		return reportFileESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getEventsToInclude() {
		return eventsToInclude;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEventsToInclude(String newEventsToInclude) {
		String oldEventsToInclude = eventsToInclude;
		eventsToInclude = newEventsToInclude;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__EVENTS_TO_INCLUDE, oldEventsToInclude, eventsToInclude));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getFieldsToInclude() {
		return fieldsToInclude;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setFieldsToInclude(String newFieldsToInclude) {
		String oldFieldsToInclude = fieldsToInclude;
		fieldsToInclude = newFieldsToInclude;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__FIELDS_TO_INCLUDE, oldFieldsToInclude, fieldsToInclude));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getFunctionsToInclude() {
		return functionsToInclude;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setFunctionsToInclude(String newFunctionsToInclude) {
		String oldFunctionsToInclude = functionsToInclude;
		functionsToInclude = newFunctionsToInclude;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__FUNCTIONS_TO_INCLUDE, oldFunctionsToInclude, functionsToInclude));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLloOption() {
		return lloOption;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLloOption(String newLloOption) {
		String oldLloOption = lloOption;
		lloOption = newLloOption;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__LLO_OPTION, oldLloOption, lloOption));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getViolationFile() {
		return violationFile;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setViolationFile(String newViolationFile) {
		String oldViolationFile = violationFile;
		violationFile = newViolationFile;
		boolean oldViolationFileESet = violationFileESet;
		violationFileESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ORACLE_TYPE__VIOLATION_FILE, oldViolationFile, violationFile, !oldViolationFileESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetViolationFile() {
		String oldViolationFile = violationFile;
		boolean oldViolationFileESet = violationFileESet;
		violationFile = VIOLATION_FILE_EDEFAULT;
		violationFileESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.ORACLE_TYPE__VIOLATION_FILE, oldViolationFile, VIOLATION_FILE_EDEFAULT, oldViolationFileESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetViolationFile() {
		return violationFileESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.ORACLE_TYPE__GHCR_TOPTS:
				return getGHCRTopts();
			case ProjectPackage.ORACLE_TYPE__ORACLE_FILE:
				return getOracleFile();
			case ProjectPackage.ORACLE_TYPE__REPORT_FILE:
				return getReportFile();
			case ProjectPackage.ORACLE_TYPE__EVENTS_TO_INCLUDE:
				return getEventsToInclude();
			case ProjectPackage.ORACLE_TYPE__FIELDS_TO_INCLUDE:
				return getFieldsToInclude();
			case ProjectPackage.ORACLE_TYPE__FUNCTIONS_TO_INCLUDE:
				return getFunctionsToInclude();
			case ProjectPackage.ORACLE_TYPE__LLO_OPTION:
				return getLloOption();
			case ProjectPackage.ORACLE_TYPE__VIOLATION_FILE:
				return getViolationFile();
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
			case ProjectPackage.ORACLE_TYPE__GHCR_TOPTS:
				setGHCRTopts((String)newValue);
				return;
			case ProjectPackage.ORACLE_TYPE__ORACLE_FILE:
				setOracleFile((String)newValue);
				return;
			case ProjectPackage.ORACLE_TYPE__REPORT_FILE:
				setReportFile((String)newValue);
				return;
			case ProjectPackage.ORACLE_TYPE__EVENTS_TO_INCLUDE:
				setEventsToInclude((String)newValue);
				return;
			case ProjectPackage.ORACLE_TYPE__FIELDS_TO_INCLUDE:
				setFieldsToInclude((String)newValue);
				return;
			case ProjectPackage.ORACLE_TYPE__FUNCTIONS_TO_INCLUDE:
				setFunctionsToInclude((String)newValue);
				return;
			case ProjectPackage.ORACLE_TYPE__LLO_OPTION:
				setLloOption((String)newValue);
				return;
			case ProjectPackage.ORACLE_TYPE__VIOLATION_FILE:
				setViolationFile((String)newValue);
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
			case ProjectPackage.ORACLE_TYPE__GHCR_TOPTS:
				setGHCRTopts(GHCR_TOPTS_EDEFAULT);
				return;
			case ProjectPackage.ORACLE_TYPE__ORACLE_FILE:
				unsetOracleFile();
				return;
			case ProjectPackage.ORACLE_TYPE__REPORT_FILE:
				unsetReportFile();
				return;
			case ProjectPackage.ORACLE_TYPE__EVENTS_TO_INCLUDE:
				setEventsToInclude(EVENTS_TO_INCLUDE_EDEFAULT);
				return;
			case ProjectPackage.ORACLE_TYPE__FIELDS_TO_INCLUDE:
				setFieldsToInclude(FIELDS_TO_INCLUDE_EDEFAULT);
				return;
			case ProjectPackage.ORACLE_TYPE__FUNCTIONS_TO_INCLUDE:
				setFunctionsToInclude(FUNCTIONS_TO_INCLUDE_EDEFAULT);
				return;
			case ProjectPackage.ORACLE_TYPE__LLO_OPTION:
				setLloOption(LLO_OPTION_EDEFAULT);
				return;
			case ProjectPackage.ORACLE_TYPE__VIOLATION_FILE:
				unsetViolationFile();
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
			case ProjectPackage.ORACLE_TYPE__GHCR_TOPTS:
				return GHCR_TOPTS_EDEFAULT == null ? gHCRTopts != null : !GHCR_TOPTS_EDEFAULT.equals(gHCRTopts);
			case ProjectPackage.ORACLE_TYPE__ORACLE_FILE:
				return isSetOracleFile();
			case ProjectPackage.ORACLE_TYPE__REPORT_FILE:
				return isSetReportFile();
			case ProjectPackage.ORACLE_TYPE__EVENTS_TO_INCLUDE:
				return EVENTS_TO_INCLUDE_EDEFAULT == null ? eventsToInclude != null : !EVENTS_TO_INCLUDE_EDEFAULT.equals(eventsToInclude);
			case ProjectPackage.ORACLE_TYPE__FIELDS_TO_INCLUDE:
				return FIELDS_TO_INCLUDE_EDEFAULT == null ? fieldsToInclude != null : !FIELDS_TO_INCLUDE_EDEFAULT.equals(fieldsToInclude);
			case ProjectPackage.ORACLE_TYPE__FUNCTIONS_TO_INCLUDE:
				return FUNCTIONS_TO_INCLUDE_EDEFAULT == null ? functionsToInclude != null : !FUNCTIONS_TO_INCLUDE_EDEFAULT.equals(functionsToInclude);
			case ProjectPackage.ORACLE_TYPE__LLO_OPTION:
				return LLO_OPTION_EDEFAULT == null ? lloOption != null : !LLO_OPTION_EDEFAULT.equals(lloOption);
			case ProjectPackage.ORACLE_TYPE__VIOLATION_FILE:
				return isSetViolationFile();
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
		result.append(" (gHCRTopts: ");
		result.append(gHCRTopts);
		result.append(", oracleFile: ");
		if (oracleFileESet) result.append(oracleFile); else result.append("<unset>");
		result.append(", reportFile: ");
		if (reportFileESet) result.append(reportFile); else result.append("<unset>");
		result.append(", eventsToInclude: ");
		result.append(eventsToInclude);
		result.append(", fieldsToInclude: ");
		result.append(fieldsToInclude);
		result.append(", functionsToInclude: ");
		result.append(functionsToInclude);
		result.append(", lloOption: ");
		result.append(lloOption);
		result.append(", violationFile: ");
		if (violationFileESet) result.append(violationFile); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //OracleTypeImpl
