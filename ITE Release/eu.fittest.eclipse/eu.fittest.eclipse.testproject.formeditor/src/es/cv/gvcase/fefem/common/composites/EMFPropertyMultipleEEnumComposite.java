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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.Enumerator;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.internal.Messages;

public abstract class EMFPropertyMultipleEEnumComposite extends
		EMFPropertyMultipleEReferenceComposite {

	public EMFPropertyMultipleEEnumComposite(Composite parent, int style,
			FormToolkit toolkit, EObject eobject, FEFEMPage page) {
		super(parent, style, toolkit, eobject, page);
	}

	public EMFPropertyMultipleEEnumComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyMultipleEReferenceComposite#getChooserLabelProvider()
	 */
	@Override
	protected ILabelProvider getChooserLabelProvider() {
		return new LabelProvider(){

			@Override
			public String getText(Object element) {
				if(element instanceof EEnumLiteral){
					return ((EEnumLiteral) element).getLiteral();
				}else
					return element.toString();
			}
			
		};
	}
	
	

	@Override
	protected void createAddAndRemoveButtons(Composite container,
			FormToolkit toolkit) {
		
		createAddButton(container,Messages.EMFPropertyMultipleEENumComposite_AddReference, toolkit);
		
		createRemoveButton(container,Messages.EMFPropertyMultipleEENumComposite_RemoveReference, toolkit);
		
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyMultipleEReferenceComposite#updateChoices()
	 */
	@Override
	protected void updateChoices() {
		if(getEEnum() != null){
			EEnum dt = getEEnum();

				List<EObject> lobjs = new ArrayList<EObject>();
				EList<EEnumLiteral> eLobjs = ((EEnum) dt).getELiterals();
				Iterator<EEnumLiteral> iobjs = eLobjs.iterator();
				while(iobjs.hasNext()){
					EEnumLiteral e = iobjs.next();
					lobjs.add(e);
				}
				this.setChoices(lobjs);

		}

	}

	private EEnum getEEnum(){
		if(getFeature() instanceof EAttribute)
			return (EEnum) ((EAttribute) getFeature()).getEAttributeType();
		else
			return null;
				
	}
	
	@Override
	protected void handleSelection(Object[] selection) {
		List<Enumerator> selEnum = new ArrayList<Enumerator>();
		for(int i=0;i<selection.length;i++)
			selEnum.add(((EEnumLiteral) selection[i]).getInstance());
		modelObservable.addAll(selEnum);
		getPage().setDirty(true);
		getViewer().setSelection(new StructuredSelection(selection));
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFContainedCollectionEditionComposite#getCellModifier(es.cv.gvcase.fefem.common.FEFEMPage)
	 */
	@Override
	protected ICellModifier getCellModifier(FEFEMPage page) {
		return null;
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFContainedCollectionEditionComposite#getColumNames()
	 */
	@Override
	protected String[] getColumNames() {
		return new String[] {this.getLabelText()};
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFContainedCollectionEditionComposite#getLabelProvider()
	 */
	@Override
	protected CellLabelProvider getLabelProvider() {
		return new CellLabelProvider(){
			
			@Override
			public void update(ViewerCell cell) {

				if (cell.getElement() instanceof EEnumLiteral){
					EEnumLiteral e = (EEnumLiteral) cell.getElement();
					cell.setText(e.getLiteral());
				}		

				else
					cell.setText(cell.getElement().toString());

			}
			
		};
	}


}
