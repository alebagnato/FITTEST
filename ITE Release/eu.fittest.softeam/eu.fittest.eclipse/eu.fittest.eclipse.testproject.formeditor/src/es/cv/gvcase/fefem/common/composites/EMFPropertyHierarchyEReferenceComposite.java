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

import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.conversion.Converter;
import org.eclipse.core.databinding.conversion.IConverter;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.internal.Messages;
import es.cv.gvcase.fefem.common.widgets.SearchableTree;

/**
 * This composite is similar to EMFPropertyEReferenceComposite but the elements in the
 * selection list are organized as a searchable hierarchy collection.
 * The ContentProvider, Input and LabelProvider of internal tree viewer are delegated to descendants
 * in order to support any kind of hierarchy structure.
 * 
 *  @author Jose Manuel García Valladolid
 */
public abstract class EMFPropertyHierarchyEReferenceComposite extends
		EMFPropertyComposite {

	protected Text selectionText;
	protected Button popupButton;

	protected SearchableTree searchableTree;
	protected Shell treeWindow;
	protected Button okButton;
	protected Button cancelButton;
	
	/**
	 * @param parent
	 * @param style
	 * @param toolkit
	 * @param object
	 * @param page
	 */
	public EMFPropertyHierarchyEReferenceComposite(Composite parent,
			int style, FormToolkit toolkit, EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
	}

	/**
	 * @param parent
	 * @param style
	 * @param toolkit
	 * @param viewer
	 * @param page
	 */
	public EMFPropertyHierarchyEReferenceComposite(Composite parent,
			int style, FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#createWidgets(org.eclipse.ui.forms.widgets.FormToolkit)
	 */
	@Override
	protected void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(3, false));
		
		createLabel(toolkit);
		
		selectionText = toolkit.createText(this, this.getLabelText(), SWT.FLAT | SWT.BORDER | SWT.READ_ONLY);
		selectionText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		popupButton = toolkit.createButton(this, "...", SWT.NONE);
		popupButton.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL));
		
		popupButton.addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				
				treeWindow = new Shell(getShell()); 
				treeWindow.setText(Messages.EMFPropertyHierarchyEReferenceComposite_WindowTreeTitle);
				treeWindow.setSize(400,300);
				treeWindow.setLayout(new GridLayout(2, false));
				
				searchableTree = new SearchableTree(treeWindow, SWT.NONE);
				searchableTree.setContentProvider(getHierarchyContentProvider());
				searchableTree.setLabelProvider(getHierarchyLabelProvider());
				searchableTree.setInput(getHierarchyInput());
				GridData tgd = new GridData(GridData.FILL_BOTH);
				tgd.horizontalSpan = 2;
				searchableTree.setLayoutData(tgd);
				searchableTree.getTreeViewer().addSelectionChangedListener(new ISelectionChangedListener(){

					public void selectionChanged(SelectionChangedEvent event) {
						Object osel = ((IStructuredSelection) searchableTree.getTreeViewer().getSelection()).getFirstElement();
						if(isSelectable(osel))
							okButton.setEnabled(true);
						else
							okButton.setEnabled(false);
					}
					
				});
				
				Group buttonsGroup = new Group(treeWindow, SWT.END );
				GridData gdbg = new GridData(GridData.FILL_HORIZONTAL);
				gdbg.horizontalSpan = 2;
				buttonsGroup.setLayoutData(gdbg);
				buttonsGroup.setLayout(new GridLayout(2, false));
				
				okButton = new Button(buttonsGroup, SWT.NONE);
				okButton.setText(Messages.EMFPropertyHierarchyEReferenceComposite_WindowTreeOkButton);
				GridData bokgd = new GridData(GridData.FILL_HORIZONTAL);
				okButton.setLayoutData(bokgd);
				okButton.setEnabled(false);
				
				cancelButton = new Button(buttonsGroup, SWT.NONE);
				cancelButton.setText( Messages.EMFPropertyHierarchyEReferenceComposite_WindowTreeCancelButton);
				GridData bcancelgd = new GridData(GridData.FILL_HORIZONTAL);
				cancelButton.setLayoutData(bcancelgd);
				
				okButton.addSelectionListener(new SelectionAdapter(){

					@Override
					public void widgetSelected(SelectionEvent e) {
						if(searchableTree.getTreeViewer().getSelection() != null){
							Object osel = ((IStructuredSelection) searchableTree.getTreeViewer().getSelection()).getFirstElement();
							if(isSelectable(osel)){
								
								modelObservable.setValue(osel);
								getPage().setDirty(true);
								
							}
						}
						treeWindow.close();
					}
					
				});
				
				cancelButton.addSelectionListener(new SelectionAdapter(){

					@Override
					public void widgetSelected(SelectionEvent e) {
						treeWindow.close();
					}
					
				});
				
				treeWindow.open();
			}
			
		});
		
		

	}

	
	private class SearchableTreeConverter extends Converter {
		
		public SearchableTreeConverter(Object fromType, Object toType) {
			super(fromType, toType);
		}

		public Object convert(Object fromObject) {
			
			if(fromObject instanceof EObject) {
				return getHierarchyLabelProvider().getText(fromObject);
			}
			
			return null;
		}
	}

	protected IConverter getConverter() {
		return new SearchableTreeConverter(null, null);
	}

	@Override
	protected UpdateValueStrategy getModelToTargetUpdateValueStrategy() {
		UpdateValueStrategy uvs = new UpdateValueStrategy();
		if(getConverter() != null) {
			uvs.setConverter(getConverter());
		}
		return uvs;
	}

	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#getTargetObservable()
	 */
	@Override
	protected IObservableValue getTargetObservable() {
		return SWTObservables.observeText(selectionText, SWT.Modify);
	}

	protected abstract ITreeContentProvider getHierarchyContentProvider();
	protected abstract Object getHierarchyInput();
	protected abstract ILabelProvider getHierarchyLabelProvider();
	protected abstract boolean isSelectable(Object element);
}
