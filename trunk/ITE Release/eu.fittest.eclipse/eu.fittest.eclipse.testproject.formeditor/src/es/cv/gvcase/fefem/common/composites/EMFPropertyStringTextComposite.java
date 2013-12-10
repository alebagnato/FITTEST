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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;

/**
 * An implementation of a composite with a multiline text field.
 * Invoke bindFeatureToWidget to bind the Text with the feature
 * 
 * @author Mario Cervera
 */
public abstract class EMFPropertyStringTextComposite extends EMFPropertyStringComposite {

	
	
	public EMFPropertyStringTextComposite(Composite parent, int style,
			FormToolkit toolkit, EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
	}
	
	public EMFPropertyStringTextComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
	}

	@Override
	public int getStyle() {
		
		return SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.V_SCROLL;
	}
	
	@Override
	protected void createWidgets(FormToolkit toolkit) {
		
		super.createWidgets(toolkit);
		
		GridData gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
		gd.widthHint = getStandardLabelWidth(this, new String[] {getLabelText()});
		getLabel().setLayoutData(gd);
		
		GridData gd2 = new GridData(GridData.VERTICAL_ALIGN_BEGINNING | GridData.FILL_HORIZONTAL);
		gd2.heightHint = 55;
		getText().setLayoutData(gd2);
	}
	
	@Override
	public void setLabelText(String text) {
		
		super.setLabelText(text);
		
		GridData gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
		gd.widthHint = getStandardLabelWidth(this, new String[] {text});
		getLabel().setLayoutData(gd);
	}

}
