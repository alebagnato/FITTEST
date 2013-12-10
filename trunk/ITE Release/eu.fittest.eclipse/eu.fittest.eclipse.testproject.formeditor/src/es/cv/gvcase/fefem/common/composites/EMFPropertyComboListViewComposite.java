/***************************************************************************
* Copyright (c) 2008 Conselleria de Infraestructuras y Transporte,
* Generalitat de la Comunitat Valenciana . All rights reserved. This program
* and the accompanying materials are made available under the terms of the
* Eclipse Public License v1.0 which accompanies this distribution, and is
* available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors: Jose Manuel Garc��a Valladolid (CIT) - Initial API and implementation
*
**************************************************************************/

package es.cv.gvcase.fefem.common.composites;

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.databinding.EMFObservables;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;


/**
 * Creates a readonly combo list composite very usefull for creating the master part of a master/detail UI patterns.
 * The candidate elements are automatically obtained by the given structural feature.
 * The descendant classes only needs to provide a LabelProvider for these elements.
 *
 * @author Jose Manuel Garc��a Valladolid
 */
public abstract class EMFPropertyComboListViewComposite extends EMFPropertyComposite {


	
	protected ComboViewer viewer;
	protected Combo combo;
	
	/**
	 * @param parent
	 * @param style 
	 * @param toolkit
	 * @param object
	 * @param page
	 */
	public EMFPropertyComboListViewComposite(Composite parent, int style,
			FormToolkit toolkit, EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
	}

	/**
	 * @param parent
	 * @param style
	 * @param toolkit
	 * @param viewer
	 * @param page
	 */
	public EMFPropertyComboListViewComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
		
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#createWidgets(org.eclipse.ui.forms.widgets.FormToolkit)
	 */
	@Override
	protected void createWidgets(FormToolkit toolkit) {
		
		this.setLayout(new GridLayout(2, false));
		
		Label label = toolkit.createLabel(this, getLabelText()); 
		GridData gd = new GridData(GridData.BEGINNING);
		label.setLayoutData(gd);
		
		

			combo = new Combo(this, SWT.READ_ONLY );
			combo.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_BEGINNING));
			
			viewer = new ComboViewer(combo);
			
			toolkit.adapt(combo);
		toolkit.adapt(this);

	}

	@Override	
	protected void bindFeatureToWidget() {
		if(getPage().getEditor().getModel() == null ||
				getPage().getEditor().getEditingDomain() == null) return;
		
		
		IObservableList candidatesObservable;
		
		if (! isDetailComposite()) {
			candidatesObservable =  EMFObservables.observeList(this.getEObject(), getFeature());			
			
		} else {
			IObservableValue selectionObservable = ViewersObservables
			.observeSingleSelection(this.getMasterViewer());
			
			candidatesObservable = EMFObservables.observeDetailList(Realm.getDefault(),
						selectionObservable, getFeature());

		}

		
		ObservableListContentProvider contentProvider = new ObservableListContentProvider();
		viewer.setContentProvider(contentProvider);
		viewer.setInput(candidatesObservable);
		if (getLabelProvider()!=null){
			viewer.setLabelProvider(getLabelProvider());
		}
	}
	
	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#getTargetObservable()
	 */
	@Override
	protected IObservableValue getTargetObservable() {
		return ViewersObservables.observeSingleSelection(viewer);
	}

	
	public ComboViewer getViewer() {
		return viewer;
	}

		
	protected abstract ILabelProvider getLabelProvider();
}
