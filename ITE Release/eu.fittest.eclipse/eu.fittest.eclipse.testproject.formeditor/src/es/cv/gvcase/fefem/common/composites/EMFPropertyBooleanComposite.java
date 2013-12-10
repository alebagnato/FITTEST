/***************************************************************************
* Copyright (c) 2008 Conselleria de Infraestructuras y Transporte,
* Generalitat de la Comunitat Valenciana . All rights reserved. This program
* and the accompanying materials are made available under the terms of the
* Eclipse Public License v1.0 which accompanies this distribution, and is
* available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors: Jose Manuel García Valladolid (CIT) - Initial API and implementation
*
**************************************************************************/

package es.cv.gvcase.fefem.common.composites;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;

public abstract class EMFPropertyBooleanComposite extends EMFPropertyComposite {

	/** The checkbox control for the composite. */
	private Button checkbox;
	
	/**
	 * @param parent
	 * @param style
	 * @param toolkit
	 * @param object
	 * @param page
	 */
	public EMFPropertyBooleanComposite(Composite parent, int style,
			FormToolkit toolkit, EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param parent
	 * @param style
	 * @param toolkit
	 * @param viewer
	 * @param page
	 */
	public EMFPropertyBooleanComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#createWidgets(org.eclipse.ui.forms.widgets.FormToolkit)
	 */
	@Override
	protected void createWidgets(FormToolkit toolkit) {
		
		this.setLayout(new GridLayout(2, false));
		createLabel(toolkit);

		checkbox = toolkit.createButton(this, "", getStyle());
		checkbox.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		toolkit.adapt(this);
	}

	
	@Override
	public int getStyle() {
		return SWT.CHECK;
	}

	
	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#getTargetObservable()
	 */
	@Override
	protected IObservableValue getTargetObservable() {
		return SWTObservables.observeSelection(checkbox);
	}

	public Button getCheckbox() {
		return checkbox;
	}

	@Override
	public void setEnabled(boolean enabled) {
		
		super.setEnabled(enabled);
		checkbox.setEnabled(enabled);
	}

	
}
