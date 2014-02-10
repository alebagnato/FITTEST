/**
 */
package eu.fittest.test.project.provider;


import eu.fittest.test.project.GAParameterType;
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
 * This is the item provider adapter for a {@link eu.fittest.test.project.GAParameterType} object.
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class GAParameterTypeItemProvider
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
	public GAParameterTypeItemProvider(AdapterFactory adapterFactory) {
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

			addPopulationSizePropertyDescriptor(object);
			addChromosomeLengthPropertyDescriptor(object);
			addMaxNumberOfGenerationsPropertyDescriptor(object);
			addMutationRatePropertyDescriptor(object);
			addTimeBudgetPropertyDescriptor(object);
			addStopPortPropertyDescriptor(object);
		}
		return itemPropertyDescriptors;
	}

	/**
	 * This adds a property descriptor for the Population Size feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addPopulationSizePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_GAParameterType_populationSize_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_GAParameterType_populationSize_feature", "_UI_GAParameterType_type"),
				 ProjectPackage.Literals.GA_PARAMETER_TYPE__POPULATION_SIZE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.INTEGRAL_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Chromosome Length feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addChromosomeLengthPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_GAParameterType_chromosomeLength_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_GAParameterType_chromosomeLength_feature", "_UI_GAParameterType_type"),
				 ProjectPackage.Literals.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.INTEGRAL_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Max Number Of Generations feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addMaxNumberOfGenerationsPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_GAParameterType_maxNumberOfGenerations_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_GAParameterType_maxNumberOfGenerations_feature", "_UI_GAParameterType_type"),
				 ProjectPackage.Literals.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.INTEGRAL_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Mutation Rate feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addMutationRatePropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_GAParameterType_mutationRate_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_GAParameterType_mutationRate_feature", "_UI_GAParameterType_type"),
				 ProjectPackage.Literals.GA_PARAMETER_TYPE__MUTATION_RATE,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.REAL_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Time Budget feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addTimeBudgetPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_GAParameterType_timeBudget_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_GAParameterType_timeBudget_feature", "_UI_GAParameterType_type"),
				 ProjectPackage.Literals.GA_PARAMETER_TYPE__TIME_BUDGET,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.INTEGRAL_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This adds a property descriptor for the Stop Port feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addStopPortPropertyDescriptor(Object object) {
		itemPropertyDescriptors.add
			(createItemPropertyDescriptor
				(((ComposeableAdapterFactory)adapterFactory).getRootAdapterFactory(),
				 getResourceLocator(),
				 getString("_UI_GAParameterType_stopPort_feature"),
				 getString("_UI_PropertyDescriptor_description", "_UI_GAParameterType_stopPort_feature", "_UI_GAParameterType_type"),
				 ProjectPackage.Literals.GA_PARAMETER_TYPE__STOP_PORT,
				 true,
				 false,
				 false,
				 ItemPropertyDescriptor.INTEGRAL_VALUE_IMAGE,
				 null,
				 null));
	}

	/**
	 * This returns GAParameterType.gif.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object getImage(Object object) {
		return overlayImage(object, getResourceLocator().getImage("full/obj16/GAParameterType"));
	}

	/**
	 * This returns the label text for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getText(Object object) {
		GAParameterType gaParameterType = (GAParameterType)object;
		return getString("_UI_GAParameterType_type") + " " + gaParameterType.getPopulationSize();
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

		switch (notification.getFeatureID(GAParameterType.class)) {
			case ProjectPackage.GA_PARAMETER_TYPE__POPULATION_SIZE:
			case ProjectPackage.GA_PARAMETER_TYPE__CHROMOSOME_LENGTH:
			case ProjectPackage.GA_PARAMETER_TYPE__MAX_NUMBER_OF_GENERATIONS:
			case ProjectPackage.GA_PARAMETER_TYPE__MUTATION_RATE:
			case ProjectPackage.GA_PARAMETER_TYPE__TIME_BUDGET:
			case ProjectPackage.GA_PARAMETER_TYPE__STOP_PORT:
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
