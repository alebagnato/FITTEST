/**
 */
package eu.fittest.test.project.provider;


import eu.fittest.test.project.ProjectFactory;
import eu.fittest.test.project.ProjectPackage;
import eu.fittest.test.project.TestProjectType;

import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.ResourceLocator;

import org.eclipse.emf.ecore.EStructuralFeature;

import org.eclipse.emf.edit.provider.IEditingDomainItemProvider;
import org.eclipse.emf.edit.provider.IItemLabelProvider;
import org.eclipse.emf.edit.provider.IItemPropertySource;
import org.eclipse.emf.edit.provider.IStructuredItemContentProvider;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.emf.edit.provider.ViewerNotification;

/**
 * This is the item provider adapter for a {@link eu.fittest.test.project.TestProjectType} object.
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class TestProjectTypeItemProvider
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
	public TestProjectTypeItemProvider(AdapterFactory adapterFactory) {
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

		}
		return itemPropertyDescriptors;
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
			childrenFeatures.add(ProjectPackage.Literals.TEST_PROJECT_TYPE__GENERAL);
			childrenFeatures.add(ProjectPackage.Literals.TEST_PROJECT_TYPE__LOGGING);
			childrenFeatures.add(ProjectPackage.Literals.TEST_PROJECT_TYPE__MODEL_INFERENCE);
			childrenFeatures.add(ProjectPackage.Literals.TEST_PROJECT_TYPE__TEST_GENERATION);
			childrenFeatures.add(ProjectPackage.Literals.TEST_PROJECT_TYPE__ORACLE);
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
	 * This returns TestProjectType.gif.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object getImage(Object object) {
		return overlayImage(object, getResourceLocator().getImage("full/obj16/TestProjectType"));
	}

	/**
	 * This returns the label text for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getText(Object object) {
		return getString("_UI_TestProjectType_type");
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

		switch (notification.getFeatureID(TestProjectType.class)) {
			case ProjectPackage.TEST_PROJECT_TYPE__GENERAL:
			case ProjectPackage.TEST_PROJECT_TYPE__LOGGING:
			case ProjectPackage.TEST_PROJECT_TYPE__MODEL_INFERENCE:
			case ProjectPackage.TEST_PROJECT_TYPE__TEST_GENERATION:
			case ProjectPackage.TEST_PROJECT_TYPE__ORACLE:
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
				(ProjectPackage.Literals.TEST_PROJECT_TYPE__GENERAL,
				 ProjectFactory.eINSTANCE.createGeneralType()));

		newChildDescriptors.add
			(createChildParameter
				(ProjectPackage.Literals.TEST_PROJECT_TYPE__LOGGING,
				 ProjectFactory.eINSTANCE.createLoggingType()));

		newChildDescriptors.add
			(createChildParameter
				(ProjectPackage.Literals.TEST_PROJECT_TYPE__MODEL_INFERENCE,
				 ProjectFactory.eINSTANCE.createModelInferenceType()));

		newChildDescriptors.add
			(createChildParameter
				(ProjectPackage.Literals.TEST_PROJECT_TYPE__TEST_GENERATION,
				 ProjectFactory.eINSTANCE.createTestGenerationType()));

		newChildDescriptors.add
			(createChildParameter
				(ProjectPackage.Literals.TEST_PROJECT_TYPE__ORACLE,
				 ProjectFactory.eINSTANCE.createOracleType()));
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
