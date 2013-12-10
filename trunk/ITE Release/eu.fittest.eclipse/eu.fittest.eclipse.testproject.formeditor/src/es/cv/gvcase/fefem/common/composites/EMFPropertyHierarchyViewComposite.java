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

import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.masterdetail.IObservableFactory;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.databinding.EMFObservables;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.jface.databinding.viewers.ObservableListTreeContentProvider;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.databinding.EMFTreeStructuredAdvisor;


/**
 * Creates a readonly tree composite very usefull for creating the master part of a master/detail UI patterns.
 * This composite, as EMFContainedHierarchicalCollectionEditionComposite does, is based on ecore Structural Features in which
 * the root elements of the tree are binded to a given feature and the parent/child reflexive relation is determined by another
 * given reflexive feature.
 * 
 * @author Jose Manuel García Valladolid
 */
public abstract class EMFPropertyHierarchyViewComposite extends
		EMFPropertyComposite {

	private TreeViewer viewer;
	private Tree tree;
	
	/**
	 * @param parent
	 * @param style
	 * @param toolkit
	 * @param object
	 * @param page
	 */
	public EMFPropertyHierarchyViewComposite(Composite parent,
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
	public EMFPropertyHierarchyViewComposite(Composite parent,
			int style, FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
	}

	
	@Override
	protected void bindFeatureToWidget() {
		if(getPage().getEditor().getModel() == null ||
				getPage().getEditor().getEditingDomain() == null) return;
		
		IObservableFactory fac = new IObservableFactory(){

			public IObservable createObservable(Object target) {
				
				if(target instanceof IObservableList){
					return ((IObservableList) target);
				}else{
					
					// Now we get the reflexive EObject node
					IObservableList childrenModelObservable = EMFObservables.observeList((EObject) target, getReflexiveFeature());
					return childrenModelObservable;
				}
			}
		};
		
		EMFTreeStructuredAdvisor adv = new EMFTreeStructuredAdvisor(getReflexiveFeature());
		ObservableListTreeContentProvider contentProvider = new ObservableListTreeContentProvider(fac, adv);
		getViewer().setContentProvider(contentProvider);

		IObservableList candidatesObservable;
		if (! isDetailComposite()) {
			candidatesObservable = EMFObservables.observeList(this.getEObject(),this.getFeature());			
			
		} else {
			IObservableValue selectionObservable = ViewersObservables
			.observeSingleSelection(this.getMasterViewer());
			
			candidatesObservable = EMFObservables.observeDetailList(Realm.getDefault(),
					selectionObservable, this.getFeature());

		}		
		
		getViewer().setInput(candidatesObservable);
		
		if (getLabelProvider()!=null){
			getViewer().setLabelProvider(getLabelProvider());
		}
	}

	protected abstract EStructuralFeature getReflexiveFeature();
	protected abstract ILabelProvider getLabelProvider();
	
	@Override
	protected void createWidgets(FormToolkit toolkit) {
		
		this.setLayout(new GridLayout(2, false));
		
		Label label = toolkit.createLabel(this, getLabelText()); 
		GridData gd = new GridData(GridData.BEGINNING);
		label.setLayoutData(gd);
		
		

			tree = new Tree(this, SWT.READ_ONLY | getStyle());
			tree.setLayoutData(new GridData(GridData.FILL_BOTH));
			
			viewer = new TreeViewer(tree);
			
			toolkit.adapt(this);
		
	}

	@Override
	protected IObservableValue getTargetObservable() {
		return ViewersObservables.observeSingleSelection(viewer);
	}

	public TreeViewer getViewer() {
		return viewer;
	}

	@Override
	public int getStyle() {
		return (SWT.BORDER);
	}
	

}
