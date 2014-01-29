/**
 */
package eu.fittest.test.project.impl;

import eu.fittest.test.project.GeneralType;
import eu.fittest.test.project.ProjectPackage;
import eu.fittest.test.project.SUTTechnologyType;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>General Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link eu.fittest.test.project.impl.GeneralTypeImpl#getType <em>Type</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GeneralTypeImpl#getBaseURL <em>Base URL</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GeneralTypeImpl#getEntryPage <em>Entry Page</em>}</li>
 *   <li>{@link eu.fittest.test.project.impl.GeneralTypeImpl#getServerFolder <em>Server Folder</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class GeneralTypeImpl extends EObjectImpl implements GeneralType {
	/**
	 * The default value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final SUTTechnologyType TYPE_EDEFAULT = SUTTechnologyType.FLASH_LITERAL;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected SUTTechnologyType type = TYPE_EDEFAULT;

	/**
	 * This is true if the Type attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean typeESet;

	/**
	 * The default value of the '{@link #getBaseURL() <em>Base URL</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBaseURL()
	 * @generated
	 * @ordered
	 */
	protected static final String BASE_URL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getBaseURL() <em>Base URL</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBaseURL()
	 * @generated
	 * @ordered
	 */
	protected String baseURL = BASE_URL_EDEFAULT;

	/**
	 * The default value of the '{@link #getEntryPage() <em>Entry Page</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEntryPage()
	 * @generated
	 * @ordered
	 */
	protected static final String ENTRY_PAGE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEntryPage() <em>Entry Page</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEntryPage()
	 * @generated
	 * @ordered
	 */
	protected String entryPage = ENTRY_PAGE_EDEFAULT;

	/**
	 * The default value of the '{@link #getServerFolder() <em>Server Folder</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getServerFolder()
	 * @generated
	 * @ordered
	 */
	protected static final String SERVER_FOLDER_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getServerFolder() <em>Server Folder</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getServerFolder()
	 * @generated
	 * @ordered
	 */
	protected String serverFolder = SERVER_FOLDER_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected GeneralTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.GENERAL_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SUTTechnologyType getType() {
		return type;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setType(SUTTechnologyType newType) {
		SUTTechnologyType oldType = type;
		type = newType == null ? TYPE_EDEFAULT : newType;
		boolean oldTypeESet = typeESet;
		typeESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GENERAL_TYPE__TYPE, oldType, type, !oldTypeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetType() {
		SUTTechnologyType oldType = type;
		boolean oldTypeESet = typeESet;
		type = TYPE_EDEFAULT;
		typeESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, ProjectPackage.GENERAL_TYPE__TYPE, oldType, TYPE_EDEFAULT, oldTypeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetType() {
		return typeESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getBaseURL() {
		return baseURL;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBaseURL(String newBaseURL) {
		String oldBaseURL = baseURL;
		baseURL = newBaseURL;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GENERAL_TYPE__BASE_URL, oldBaseURL, baseURL));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getEntryPage() {
		return entryPage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEntryPage(String newEntryPage) {
		String oldEntryPage = entryPage;
		entryPage = newEntryPage;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GENERAL_TYPE__ENTRY_PAGE, oldEntryPage, entryPage));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getServerFolder() {
		return serverFolder;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setServerFolder(String newServerFolder) {
		String oldServerFolder = serverFolder;
		serverFolder = newServerFolder;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.GENERAL_TYPE__SERVER_FOLDER, oldServerFolder, serverFolder));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.GENERAL_TYPE__TYPE:
				return getType();
			case ProjectPackage.GENERAL_TYPE__BASE_URL:
				return getBaseURL();
			case ProjectPackage.GENERAL_TYPE__ENTRY_PAGE:
				return getEntryPage();
			case ProjectPackage.GENERAL_TYPE__SERVER_FOLDER:
				return getServerFolder();
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
			case ProjectPackage.GENERAL_TYPE__TYPE:
				setType((SUTTechnologyType)newValue);
				return;
			case ProjectPackage.GENERAL_TYPE__BASE_URL:
				setBaseURL((String)newValue);
				return;
			case ProjectPackage.GENERAL_TYPE__ENTRY_PAGE:
				setEntryPage((String)newValue);
				return;
			case ProjectPackage.GENERAL_TYPE__SERVER_FOLDER:
				setServerFolder((String)newValue);
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
			case ProjectPackage.GENERAL_TYPE__TYPE:
				unsetType();
				return;
			case ProjectPackage.GENERAL_TYPE__BASE_URL:
				setBaseURL(BASE_URL_EDEFAULT);
				return;
			case ProjectPackage.GENERAL_TYPE__ENTRY_PAGE:
				setEntryPage(ENTRY_PAGE_EDEFAULT);
				return;
			case ProjectPackage.GENERAL_TYPE__SERVER_FOLDER:
				setServerFolder(SERVER_FOLDER_EDEFAULT);
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
			case ProjectPackage.GENERAL_TYPE__TYPE:
				return isSetType();
			case ProjectPackage.GENERAL_TYPE__BASE_URL:
				return BASE_URL_EDEFAULT == null ? baseURL != null : !BASE_URL_EDEFAULT.equals(baseURL);
			case ProjectPackage.GENERAL_TYPE__ENTRY_PAGE:
				return ENTRY_PAGE_EDEFAULT == null ? entryPage != null : !ENTRY_PAGE_EDEFAULT.equals(entryPage);
			case ProjectPackage.GENERAL_TYPE__SERVER_FOLDER:
				return SERVER_FOLDER_EDEFAULT == null ? serverFolder != null : !SERVER_FOLDER_EDEFAULT.equals(serverFolder);
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
		result.append(" (type: ");
		if (typeESet) result.append(type); else result.append("<unset>");
		result.append(", baseURL: ");
		result.append(baseURL);
		result.append(", entryPage: ");
		result.append(entryPage);
		result.append(", serverFolder: ");
		result.append(serverFolder);
		result.append(')');
		return result.toString();
	}

} //GeneralTypeImpl
