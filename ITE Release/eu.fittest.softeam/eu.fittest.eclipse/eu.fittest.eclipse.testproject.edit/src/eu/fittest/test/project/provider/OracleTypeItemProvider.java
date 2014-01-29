/**
 */
package eu.fittest.test.project.provider;


import eu.fittest.test.project.OracleType;
import eu.fittest.test.project.ProjectPackage;

import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.ResourceLocator;

import org.eclipse.emf.edit.provider.ComposeableAdapterFactory;
import org.eclipse.emf.edit.provider.IEditingDomainItemProvider;
import org.eclipse.emf.edit.provider.IItemLabelProvider;
import org.eclipse.emf.edit.provider.IItemPropertySource;
import org.eclipse.emf.edit.provider.IStructuredItemContentProvider;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemPropertyDescriptor;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.emf.edit.provider.ViewerNotification;

/**
 * This is the item provider adapter for a {@link eu.fittest.test.project.OracleType} object.
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class OracleTypeItemProvider
	extends ItemProviderAdapter
	implements
		IEditingDomainItemProvider,
		IStructuredItemContentProvider,
		ITreeItemContentProvider,
		IItemLabelProvider,
		IItemPropertySource {
	/**
	 * This constructs an instance from a factory and a notifier.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OracleTypeItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}

	/**
	 * This returns the property descriptors for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getPropertyDescriptors(Object object) {
		if (itemPropertyDescriptors == null) {
			super.getPropertyDescriptors(object);

			addGHCRToptsPropertyDescriptor(object);
			addOracleFilePropertyDescriptor(object);
			addReportFilePropertyDescriptor(object);
			addEventsToIncludePropertyDescriptor(object);
			addFieldsToIncludePropertyDescriptor(object);
			addFunctionsToIncludePropertyDescriptor(object);
			addLloOptionPropertyDescriptor(object);
			addViolationFilePropertyDescriptor(object);
		}
		return itemPropertyDescriptors;
	}

	/**
	 * This adds a property descriptor for the GHCR Topts feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addGHCRToptsPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_gHCRTopts_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_gHCRTopts_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__GHCR_TOPTS,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Oracle File feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addOracleFilePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_oracleFile_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_oracleFile_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__ORACLE_FILE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Report File feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addReportFilePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_reportFile_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_reportFile_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__REPORT_FILE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Events To Include feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addEventsToIncludePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_eventsToInclude_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_eventsToInclude_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__EVENTS_TO_INCLUDE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Fields To Include feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addFieldsToIncludePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_fieldsToInclude_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_fieldsToInclude_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__FIELDS_TO_INCLUDE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Functions To Include feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addFunctionsToIncludePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_functionsToInclude_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_functionsToInclude_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__FUNCTIONS_TO_INCLUDE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Llo Option feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addLloOptionPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_lloOption_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_lloOption_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__LLO_OPTION,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Violation File feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addViolationFilePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_OracleType_violationFile_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_OracleType_violationFile_feature", "_UI_OracleType_type"),
				 ProjectPackage.Literals.ORACLE_TYPE__VIOLATION_FILE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This returns OracleType.gif.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object getImage(Object object) {
		return overlayImage(object, getResourceLocator().getImage("full/obj16/OracleType"));
	}

	/**
	 * This returns the label text for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getText(Object object) {
		String label = ((OracleType)object).getGHCRTopts();
		return label == null || label.length() == 0 ?
			getString("_UI_OracleType_type") :
			getString("_UI_OracleType_type") + " " + label;
	}

	/**
	 * This handles model notifications by calling {@link #updateChildren} to update any cached
	 * children and by creating a viewer notification, which it passes to {@link #fireNotifyChanged}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void notifyChanged(Notification notification) {
		updateChildren(notification);

		switch (notification.getFeatureID(OracleType.class)) {
			case ProjectPackage.ORACLE_TYPE__GHCR_TOPTS:
			case ProjectPackage.ORACLE_TYPE__ORACLE_FILE:
			case ProjectPackage.ORACLE_TYPE__REPORT_FILE:
			case ProjectPackage.ORACLE_TYPE__EVENTS_TO_INCLUDE:
			case ProjectPackage.ORACLE_TYPE__FIELDS_TO_INCLUDE:
			case ProjectPackage.ORACLE_TYPE__FUNCTIONS_TO_INCLUDE:
			case ProjectPackage.ORACLE_TYPE__LLO_OPTION:
			case ProjectPackage.ORACLE_TYPE__VIOLATION_FILE:
				fireNotifyChanged(new ViewerNotification(notification, notification.getNotifier(), false, true));
				return;
		}
		super.notifyChanged(notification);
	}

	/**
	 * This adds {@link org.eclipse.emf.edit.command.CommandParameter}s describing the children
	 * that can be created under this object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void collectNewChildDescriptors(Collection newChildDescriptors, Object object) {
		super.collectNewChildDescriptors(newChildDescriptors, object);
	}

	/**
	 * Return the resource locator for this item provider's resources.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ResourceLocator getResourceLocator() {
		return ProjectEditPlugin.INSTANCE;
	}

}
