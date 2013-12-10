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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.common.util.Enumerator;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;

/**
 * Composite that binds a EEnum ecore feature to a non-editable combo dropdown list with all
 * the Enumerator values as a selectable options
 *
 * @author Jose Manuel García Valladolid
 */
public abstract class EMFPropertyEEnumComposite extends EMFPropertyComposite {

	protected ComboViewer comboViewer;

	
	public EMFPropertyEEnumComposite(Composite parent, int style,
			FormToolkit toolkit, EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
	}

	
	public EMFPropertyEEnumComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
	}


	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#createWidgets(org.eclipse.ui.forms.widgets.FormToolkit)
	 */
	@Override
	protected void createWidgets(FormToolkit toolkit) {

			
			this.setLayout(new GridLayout(2, false));
			
			this.createLabel(toolkit);

			Combo combo = new Combo(this, SWT.READ_ONLY);
			comboViewer = new ComboViewer(combo);
		    
			if(getFeature() instanceof EAttribute){
				EDataType dt = ((EAttribute) getFeature()).getEAttributeType();
				if (dt instanceof EEnum){
					
					comboViewer.setContentProvider(new ArrayContentProvider());
					
					List<Enumerator> lEnums = new ArrayList<Enumerator>();
					Iterator<EEnumLiteral> iter = ((EEnum) dt).getELiterals().iterator();
					while(iter.hasNext()){
						EEnumLiteral el = iter.next();
						lEnums.add(el.getInstance());
					}
					comboViewer.setInput(lEnums.toArray());
					comboViewer.setLabelProvider(getLabelProvider());
				}
			}
			
//			combo.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			combo.setLayoutData(new GridData(GridData.FILL));
			toolkit.adapt(combo);
			toolkit.adapt(this);
			
		
	}

	@Override
	public int getStyle() {
		return SWT.BORDER | SWT.FLAT;
	}


	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#getTargetObservable()
	 */
	@Override
	protected IObservableValue getTargetObservable() {
		return ViewersObservables.observeSingleSelection(comboViewer);
	}

	
	public Viewer getViewer(){
		return comboViewer;
	}


	protected LabelProvider getLabelProvider(){
		return new LabelProvider(){
			
			@Override
			public String getText(Object element) {
				if(element instanceof Enumerator){
					return ((Enumerator) element).getLiteral();
				}else if(element == null)
					return "  ";
				else
					return element.toString();
			}
			
		};
	}
}
