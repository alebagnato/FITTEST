/**
 */
package eu.fittest.test.project.provider;


import eu.fittest.test.project.ProjectFactory;
import eu.fittest.test.project.ProjectPackage;
import eu.fittest.test.project.TestGenerationType;

import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.ResourceLocator;

import org.eclipse.emf.ecore.EStructuralFeature;

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
 * This is the item provider adapter for a {@link eu.fittest.test.project.TestGenerationType} object.
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class TestGenerationTypeItemProvider
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
	public TestGenerationTypeItemProvider(AdapterFactory adapterFactory) {
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

			addCteFolderPropertyDescriptor(object);
			addModelVisitStrategyPropertyDescriptor(object);
			addReduceTestSuitePropertyDescriptor(object);
			addSourcePackagePrefixPropertyDescriptor(object);
			addSeleniumDriverBrowserPropertyDescriptor(object);
			addSeleniumRemoteHostPropertyDescriptor(object);
			addSeleniumRemotePortPropertyDescriptor(object);
			addSeleniumBrowserConfigPropertyDescriptor(object);
		}
		return itemPropertyDescriptors;
	}

	/**
	 * This adds a property descriptor for the Cte Folder feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addCteFolderPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_cteFolder_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_cteFolder_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__CTE_FOLDER,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Model Visit Strategy feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addModelVisitStrategyPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_modelVisitStrategy_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_modelVisitStrategy_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Reduce Test Suite feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addReduceTestSuitePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_reduceTestSuite_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_reduceTestSuite_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.BOOLEAN_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Source Package Prefix feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addSourcePackagePrefixPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_sourcePackagePrefix_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_sourcePackagePrefix_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Selenium Driver Browser feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addSeleniumDriverBrowserPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_seleniumDriverBrowser_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_seleniumDriverBrowser_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Selenium Remote Host feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addSeleniumRemoteHostPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_seleniumRemoteHost_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_seleniumRemoteHost_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Selenium Remote Port feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addSeleniumRemotePortPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_seleniumRemotePort_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_seleniumRemotePort_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.INTEGRAL_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Selenium Browser Config feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addSeleniumBrowserConfigPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_TestGenerationType_seleniumBrowserConfig_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_TestGenerationType_seleniumBrowserConfig_feature", "_UI_TestGenerationType_type"),
				 ProjectPackage.Literals.TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This specifies how to implement {@link #getChildren} and is used to deduce an appropriate feature for an
	 * {@link org.eclipse.emf.edit.command.AddCommand}, {@link org.eclipse.emf.edit.command.RemoveCommand} or
	 * {@link org.eclipse.emf.edit.command.MoveCommand} in {@link #createCommand}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Collection getChildrenFeatures(Object object) {
		if (childrenFeatures == null) {
			super.getChildrenFeatures(object);
			childrenFeatures.add(ProjectPackage.Literals.TEST_GENERATION_TYPE__GA_PARAM);
		}
		return childrenFeatures;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EStructuralFeature getChildFeature(Object object, Object child) {
		// Check the type of the specified child object and return the proper feature to use for
		// adding (see {@link AddCommand}) it as a child.

		return super.getChildFeature(object, child);
	}

	/**
	 * This returns TestGenerationType.gif.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object getImage(Object object) {
		return overlayImage(object, getResourceLocator().getImage("full/obj16/TestGenerationType"));
	}

	/**
	 * This returns the label text for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getText(Object object) {
		String label = ((TestGenerationType)object).getCteFolder();
		return label == null || label.length() == 0 ?
			getString("_UI_TestGenerationType_type") :
			getString("_UI_TestGenerationType_type") + " " + label;
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

		switch (notification.getFeatureID(TestGenerationType.class)) {
			case ProjectPackage.TEST_GENERATION_TYPE__CTE_FOLDER:
			case ProjectPackage.TEST_GENERATION_TYPE__MODEL_VISIT_STRATEGY:
			case ProjectPackage.TEST_GENERATION_TYPE__REDUCE_TEST_SUITE:
			case ProjectPackage.TEST_GENERATION_TYPE__SOURCE_PACKAGE_PREFIX:
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_DRIVER_BROWSER:
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_HOST:
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_REMOTE_PORT:
			case ProjectPackage.TEST_GENERATION_TYPE__SELENIUM_BROWSER_CONFIG:
				fireNotifyChanged(new ViewerNotification(notification, notification.getNotifier(), false, true));
				return;
			case ProjectPackage.TEST_GENERATION_TYPE__GA_PARAM:
				fireNotifyChanged(new ViewerNotification(notification, notification.getNotifier(), true, false));
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

		newChildDescriptors.add
			(createChildParameter
				(ProjectPackage.Literals.TEST_GENERATION_TYPE__GA_PARAM,
				 ProjectFactory.eINSTANCE.createGAParameterType()));
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
