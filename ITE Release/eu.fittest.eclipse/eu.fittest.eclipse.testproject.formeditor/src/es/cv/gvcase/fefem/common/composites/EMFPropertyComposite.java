/***************************************************************************
 * Copyright (c) 2008 Conselleria de Infraestructuras y Transporte,
 * Generalitat de la Comunitat Valenciana . All rights reserved. This program
 * and the accompanying materials are made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Mario Cervera Ubeda (Integranova)
 *
 **************************************************************************/
package es.cv.gvcase.fefem.common.composites;

import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.databinding.EMFDataBindingContext;
import org.eclipse.emf.databinding.edit.EMFEditObservables;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;

/**
 * An abstract implementation of a composite intended to be sub-classed to bind
 * the feature returned by the abstract method getFeature with a widget using
 * databinding.
 * 
 * @author Mario Cervera
 */
public abstract class EMFPropertyComposite extends Composite {

	/**
	 * The standard label width when labels for sections line up on the left
	 * hand side of the composite.
	 */
	public static final int STANDARD_LABEL_WIDTH = 140;

	private FEFEMPage page;

	/** A EObject from where the property is taken */
	private EObject eObject = null;

	/**
	 * A viewer from where the EObject with the property is taken
	 * 
	 * Either the eobject or viewer variable will be used but not both
	 */
	private Viewer viewer = null;

	protected Object oldValue;

	/**
	 * The label for this section
	 */
	private Label label;

	protected IObservableValue modelObservable;

	private EMFDataBindingContext bindingContext;

	private Object lastModifiedObject = null;
	private EObject lastModifiedObjectCopy = null;

	private ISelectionChangedListener masterSelectionListener;

	public EMFPropertyComposite(Composite parent, int style,
			FormToolkit toolkit, EObject eObject, FEFEMPage page) {
		super(parent, style);
		this.eObject = eObject;
		this.page = page;

		createWidgets(toolkit);
		bindFeatureToWidget();
	}

	public EMFPropertyComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style);
		this.viewer = viewer;
		this.page = page;

		createWidgets(toolkit);
		bindFeatureToWidget();

		masterSelectionListener = new ISelectionChangedListener() {

			public void selectionChanged(SelectionChangedEvent event) {
				updateEnablement();
			}

		};

		this.getMasterViewer().addSelectionChangedListener(
				masterSelectionListener);
		this.addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {

				removeMasterViewerListener();
			}

		});

		updateEnablement();
	}

	protected abstract void createWidgets(FormToolkit toolkit);

	/**
	 * Get the standard label width when labels for sections line up on the left
	 * hand side of the composite. We line up to a fixed position, but if a
	 * string is wider than the fixed position, then we use that widest string.
	 * 
	 * @param parent
	 *            The parent composite used to create a GC.
	 * @param labels
	 *            The list of labels.
	 * @return the standard label width.
	 */
	protected int getStandardLabelWidth(Composite parent, String[] labels) {
		int standardLabelWidth = STANDARD_LABEL_WIDTH + 65;
		// GC gc = new GC(parent);
		//		int indent = gc.textExtent("XXX").x; //$NON-NLS-1$
		// for (int i = 0; i < labels.length; i++) {
		// int width = gc.textExtent(labels[i]).x;
		// if (width + indent > standardLabelWidth) {
		// standardLabelWidth = width + indent;
		// }
		// }
		// gc.dispose();
		return standardLabelWidth;
	}

	protected Label getLabel() {
		return label;
	}

	protected void createLabel(FormToolkit toolkit) {
		label = toolkit.createLabel(this, getLabelText(), SWT.WRAP);
		GridData gd = new GridData(GridData.BEGINNING);
		gd.widthHint = getStandardLabelWidth(this,
				new String[] { getLabelText() });
		label.setLayoutData(gd);
	}

	public void setLabelText(String text) {
		if (label != null) {
			label.setText(text);
			GridData gd = new GridData(GridData.BEGINNING);
			gd.widthHint = getStandardLabelWidth(this, new String[] { text });
			label.setLayoutData(gd);
		}
	}

	public FEFEMPage getPage() {
		return page;
	}

	/**
	 * The EObject containing the feature
	 * 
	 * @return the EObject
	 */
	public EObject getEObject() {
		if (isDetailComposite()) {
			if (viewer.getSelection() != null) {
				Object selection = ((StructuredSelection) viewer.getSelection())
						.getFirstElement();
				if (selection instanceof EObject) {
					return (EObject) selection;
				}
			} else {
				return null;
			}
		} else {
			return eObject;
		}
		return null;
	}

	public EditingDomain getEditingDomain() {
		return this.getPage().getEditor().getEditingDomain();
	}

	/**
	 * The Viewer containing the EObjects
	 * 
	 * @return the viewer
	 */
	protected Viewer getMasterViewer() {
		return viewer;
	}

	public boolean isDetailComposite() {
		return this.getMasterViewer() != null;
	}

	protected Object getFeatureValue() {
		EStructuralFeature feature = getFeature();
		if (feature == null || getEObject() == null)
			return null;
		
//		EList<EStructuralFeature> list =  getEObject().eClass().getEStructuralFeatures();
//		for (EStructuralFeature feature : list){
//			if (feature.equals(getFeature())){
//		return getEObject().eClass().getEStructuralFeature(feature.getName());
//			}
//		}
//		
		return getEObject().eClass().eGet(feature);
	}

	protected void handlePropertyChanged() {
		getPage().setDirty(true);
		getPage().refresh();
	}

	/**
	 * This method is used to bind the feature to its corresponding widget in
	 * the composite by using databinding
	 */
	protected void bindFeatureToWidget() {

		if (getFeature() == null)
			return;

		bindingContext = new EMFDataBindingContext();

		IObservableValue targetObservable = getTargetObservable();

		if (!isDetailComposite()) {

			modelObservable = EMFEditObservables
					.observeValue(
							this.getPage().getEditor().getEditingDomain(),
							getEObject(), getFeature());

			modelObservable.addChangeListener(new IChangeListener() {
				public void handleChange(ChangeEvent event) {
					handlePropertyChanged();
				}
			});
			if (targetObservable != null) { // A viewer has been passed through
											// the constructor
				bindingContext.bindValue(targetObservable, modelObservable,
						getTargetToModelUpdateValueStrategy(),
						getModelToTargetUpdateValueStrategy());
			}

		} else {

			IObservableValue selectionObservable = ViewersObservables
					.observeSingleSelection(getMasterViewer());

			modelObservable = EMFEditObservables.observeDetailValue(
					Realm.getDefault(), this.getPage().getEditor()
							.getEditingDomain(), selectionObservable,
					getFeature());
			if (targetObservable != null) { // A viewer has been passed through
											// the constructor
				bindingContext.bindValue(targetObservable, modelObservable,
						getTargetToModelUpdateValueStrategy(),
						getModelToTargetUpdateValueStrategy());
			}

			modelObservable.addChangeListener(new IChangeListener() {
				public void handleChange(ChangeEvent event) {
					if (lastModifiedObject != null
							&& lastModifiedObject.equals(getEObject())) {
						if (!EcoreUtil.equals(lastModifiedObjectCopy,
								getEObject())) {
							handlePropertyChanged();
							if (getEObject() != null) {
								lastModifiedObjectCopy = EcoreUtil
										.copy(getEObject());
							}
						}
					} else {
						lastModifiedObject = getEObject();
						if (getEObject() != null) {
							lastModifiedObjectCopy = EcoreUtil
									.copy(getEObject());
						}
						handleSelectionChanged();
					}
				}
			});
		}
	}

	protected void handleSelectionChanged() {
	}

	protected UpdateValueStrategy getTargetToModelUpdateValueStrategy() {
		return null;
	}

	protected UpdateValueStrategy getModelToTargetUpdateValueStrategy() {
		return null;
	}

	public boolean isEditable() {
		return true;
	}

	private void updateEnablement() {
		if (this.isDetailComposite()) {
			boolean enablement = !getMasterViewer().getSelection().isEmpty();

			this.setEnabled(enablement);
		}
	}

	private void removeMasterViewerListener() {
		if (this.isDetailComposite()) {
			this.getMasterViewer().removeSelectionChangedListener(
					masterSelectionListener);
		}
	}

	/**
	 * The feature the widget is binded with
	 * 
	 * @return the feature
	 */
	protected abstract EStructuralFeature getFeature();

	protected abstract IObservableValue getTargetObservable();

	protected abstract String getLabelText();

}
