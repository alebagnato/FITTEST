/***************************************************************************
* Copyright (c) 2008, 2009 Conselleria de Infraestructuras y Transporte,
* Generalitat de la Comunitat Valenciana, Open Canarias S.L. All rights reserved. 
* This program and the accompanying materials are made available under the terms 
* of the Eclipse Public License v1.0 which accompanies this distribution, and is
* available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors: 
* Jose Manuel Garc�a Valladolid (CIT) - Initial API and implementation
* Mario Cervera Ubeda (Prodevelop) - Added ordering functionality
* Adolfo Sanchez-Barbudo Herrera (Open Canarias)- Bug 2340
*
**************************************************************************/

package es.cv.gvcase.fefem.common.composites;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.databinding.edit.EMFEditObservables;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.command.AddCommand;
import org.eclipse.emf.edit.command.DeleteCommand;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.jface.databinding.viewers.ObservableListTreeContentProvider;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.databinding.EMFObservableTreeFactory;
import es.cv.gvcase.fefem.common.databinding.EMFTreeStructuredAdvisor;
import es.cv.gvcase.fefem.common.internal.Messages;

public abstract class EMFContainedHierarchicalCollectionEditionComposite extends EMFPropertyComposite {

	protected static final int ADDROOTELEMENT_BUTTON_ID = 0;
	protected static final int ADDCHILDELEMENT_BUTTON_ID = 1;
	protected static final int REMOVEELEMENT_BUTTON_ID = 2;
	protected static final int EXPAND_BUTTON_ID = 3;
	protected static final int COLLAPSE_BUTTON_ID = 4;
	protected static final int UP_BUTTON_ID = 5;
	protected static final int DOWN_BUTTON_ID = 6;
	
	private TreeViewer treeViewer;
	
	private Label searchLabel;
	
	private Text searchText;
	
	private Composite buttonsContainer;

	protected List<Button> addButtons;

	protected List<Button> removeButtons;
	
	protected Button upButton;
	
	protected Button downButton;
	
	protected List<Button> treeButtons;

	protected IObservableList modelObservable;
	
	public EMFContainedHierarchicalCollectionEditionComposite(Composite parent,	int style, FormToolkit toolkit, EObject eobject, FEFEMPage page) {
		super(parent, style, toolkit, eobject, page);
	}

	public EMFContainedHierarchicalCollectionEditionComposite(Composite parent, int style, FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
	}
	
		
	public TreeViewer getViewer(){
		return this.treeViewer;
	}
	
	
	protected ViewerFilter getFilter(){
		return null;
	}
	
	protected void createAddButton(Composite container, String buttonLabel, FormToolkit toolkit){
		// Add button
		Button addButton = toolkit.createButton(container, buttonLabel, SWT.NONE);
		addButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		SelectionListener addButtonListener = getAddButtonSelectionListener();
		if (addButtonListener != null){
			addButton.addSelectionListener(addButtonListener);
		}
		if(addButtons == null)
			addButtons = new ArrayList<Button>();
		this.addButtons.add(addButton);		
	}
	
	protected void createAddChildButton(Composite container, String buttonLabel, FormToolkit toolkit){
		// AddChild button
		Button addChildButton = toolkit.createButton(container, buttonLabel, SWT.NONE);
		addChildButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		SelectionListener addButtonListener = getAddChildButtonSelectionListener();
		if (addButtonListener != null){
			addChildButton.addSelectionListener(addButtonListener);
		}
		if(addButtons == null)
			addButtons = new ArrayList<Button>();
		this.addButtons.add(addChildButton);		
	}
	protected void createRemoveButton(Composite container, String buttonLabel, FormToolkit toolkit){
		//Remove button
		Button removeButton = toolkit.createButton(container, buttonLabel, SWT.NONE);
		removeButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		SelectionListener removeButtonListener = getRemoveButtonSelectionListener();
		if (removeButtonListener != null){
			removeButton.addSelectionListener(removeButtonListener);
		}
		if(removeButtons == null)
			removeButtons = new ArrayList<Button>();
		this.removeButtons.add(removeButton);
	}
	
	protected void createExpandButton(Composite container, String buttonLabel, FormToolkit toolkit){
		// Expand button
		Button expandButton = toolkit.createButton(container, buttonLabel, SWT.NONE);
		expandButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		SelectionListener expandButtonListener = getExpandButtonSelectionListener();
		if (expandButtonListener != null){
			expandButton.addSelectionListener(expandButtonListener);
		}
		if(addButtons == null)
			addButtons = new ArrayList<Button>();
		this.addButtons.add(expandButton);		
	}
	
	protected void createCollapseButton(Composite container, String buttonLabel, FormToolkit toolkit){
		//Collapse button
		Button collapseButton = toolkit.createButton(container, buttonLabel, SWT.NONE);
		collapseButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		SelectionListener collapseButtonListener = getCollapseButtonSelectionListener();
		if (collapseButtonListener != null){
			collapseButton.addSelectionListener(collapseButtonListener);
		}
		if(treeButtons == null)
			treeButtons = new ArrayList<Button>();
		this.treeButtons.add(collapseButton);
	}
	
	protected void createUpButton(Composite container, String buttonLabel, FormToolkit toolkit){
		//Up button
		Button upButton = toolkit.createButton(container, buttonLabel, SWT.NONE);
		upButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		SelectionListener upButtonListener = getUpButtonSelectionListener();
		if (upButtonListener != null){
			upButton.addSelectionListener(upButtonListener);
		}
	}
	
	protected void createDownButton(Composite container, String buttonLabel, FormToolkit toolkit){
		//Down button
		Button downButton = toolkit.createButton(container, buttonLabel, SWT.NONE);
		downButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		SelectionListener downButtonListener = getDownButtonSelectionListener();
		if (downButtonListener != null){
			downButton.addSelectionListener(downButtonListener);
		}
	}
	
	protected SelectionListener getAddButtonSelectionListener() {		
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				
				Object newElement = getNewRootElement();
				
				if(newElement != null) {					
					modelObservable.add(newElement);
					getPage().setDirty(true);
					getViewer().setSelection(new StructuredSelection(newElement));
				}
			}

		};
		return adapter;
	}
	
	protected SelectionListener getAddChildButtonSelectionListener() {		
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				
				if (getViewer().getSelection() != null){
					Object selection = ((StructuredSelection)getViewer().getSelection()).getFirstElement();
					if(selection instanceof EObject) {
						EObject o = (EObject) selection;
						Object child = getNewChildElement();
						
						if((o != null) && (child != null)){
							
							getEditingDomain().getCommandStack().execute(AddCommand.create(getEditingDomain(), o, getReflexiveFeature(), child));
							getPage().setDirty(true);
							getViewer().expandToLevel(selection, 1);
							getViewer().setSelection(new StructuredSelection(child));
						}
						
					}
				}
				
			}

		};
		return adapter;
	}
	
	/**
	 * Creates the a SelectionListener which will invoke the code to remove/delete
	 * a selected element from the tree viewer.
	 * 	 
	 * @return SelectionListener the {@link SelectionListener} which will
	 * remove/delete a selected element from the viewer.  
	 */
	protected SelectionListener getRemoveButtonSelectionListener() {
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				
				TreeViewer viewer = getViewer();				
				if(viewer.getSelection()instanceof TreeSelection) {
					TreeSelection selection = (TreeSelection) viewer.getSelection();
					List<?> elementsToDelete = selection.toList();
					if(elementsToDelete.size() > 0) {
						// We prepare the next selection, based on the element to delete
						EObject eObject = (EObject) elementsToDelete.get(0);
						TreePath path = selection.getPathsFor(eObject)[0];
						TreeSelection nextSelection = TreeSelection.EMPTY;;
						if (path.getSegmentCount() == 1) { // If it is a first level element, we will select a sibling element
							if (modelObservable.size() > 1) { // If we have more than one element 
								int pos = modelObservable.indexOf(eObject);
								nextSelection = (pos == modelObservable.size() - 1) // If it's the last first level element, we will select the previous sibling
									? new TreeSelection (new TreePath(new Object[] { modelObservable.get(pos - 1)} ))
									: new TreeSelection (new TreePath(new Object[] { modelObservable.get(pos + 1)} ));	// otherwise, we will select the next one
							}
						} else {  // If it is not a first level element, we will select its parent element
							nextSelection = new TreeSelection(path.getParentPath());
						}
						
						EditingDomain domain = getEditingDomain();
						domain.getCommandStack().execute(DeleteCommand.create(domain, elementsToDelete));
						getPage().setDirty(true);
						viewer.setSelection(nextSelection);
					}
				}
			}
		};
		return adapter;
	}
	
	protected SelectionListener getExpandButtonSelectionListener() {		
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				
				getViewer().expandAll();
			}

		};
		return adapter;
	}
	protected SelectionListener getCollapseButtonSelectionListener() {
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				
				getViewer().collapseAll();
			}

		};
		return adapter;
	}
	
	protected SelectionListener getUpButtonSelectionListener() {	
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				executeMovement(true);
			}

		};
		return adapter;
	}
	
	protected SelectionListener getDownButtonSelectionListener() {
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				executeMovement(false);
			}

		};
		return adapter;
	}
	
	private void executeMovement(boolean up) {
		if(getViewer().getSelection() instanceof StructuredSelection) {
			Object elementToMove = ((StructuredSelection)getViewer().getSelection()).getFirstElement();
			if(elementToMove instanceof EObject) {
				ISelection selection = getViewer().getSelection();
				int oldIndex = -1;
				int newIndex = -1;
				if(!isChild((EObject)elementToMove) && modelObservable != null) {
					oldIndex = modelObservable.indexOf(elementToMove);
					if(up) {
						newIndex = oldIndex == 0? oldIndex : oldIndex - 1;
					}
					else {
						newIndex = oldIndex == modelObservable.size() - 1?
								oldIndex : oldIndex + 1;
					}
					if(oldIndex != newIndex) {
						modelObservable.move(oldIndex, newIndex);
					}
				}
				else {
					IObservableList childrenModelObservable = EMFEditObservables.observeList(getPage().getEditor().getEditingDomain(),	
							((EObject)elementToMove).eContainer(), getReflexiveFeature());
					if(childrenModelObservable != null) {
						oldIndex = childrenModelObservable.indexOf(elementToMove);
						if(up) {
							newIndex = oldIndex == 0? oldIndex : oldIndex - 1;
						}
						else {
							newIndex = oldIndex == childrenModelObservable.size() - 1?
									oldIndex : oldIndex + 1;
						}
						if(oldIndex != newIndex) {
							childrenModelObservable.move(oldIndex, newIndex);
						}
					}
				}
				if(oldIndex != newIndex) {	
					getPage().setDirty(true);
					getViewer().refresh();
					getViewer().setSelection(selection);
				}
			}
		}
	}
	
	protected void createAddAndRemoveButtons(Composite container,
			FormToolkit toolkit) {
		
		createAddButton(container,getButtonLabel(ADDROOTELEMENT_BUTTON_ID), toolkit);
		
		createAddChildButton(container,getButtonLabel(ADDCHILDELEMENT_BUTTON_ID), toolkit);
		
		createRemoveButton(container,getButtonLabel(REMOVEELEMENT_BUTTON_ID), toolkit);
		
	}
	
	protected void createExpandAndCollapseButtons(Composite container,
			FormToolkit toolkit) {
		
		
		createExpandButton(container,getButtonLabel(EXPAND_BUTTON_ID), toolkit);
		
		createCollapseButton(container,getButtonLabel(COLLAPSE_BUTTON_ID), toolkit);
		
	}
	
	protected void createOrderingButtons(Composite container,
			FormToolkit toolkit) {
		
		createUpButton(container, getButtonLabel(UP_BUTTON_ID), toolkit);
		
		createDownButton(container, getButtonLabel(DOWN_BUTTON_ID), toolkit);
		
	}
	
	protected String getButtonLabel(int buttonID){
		switch (buttonID){
			case ADDROOTELEMENT_BUTTON_ID: return Messages.EMFContainedHierarchicalCollectionEditionComposite_NewRoot;
			case ADDCHILDELEMENT_BUTTON_ID: return Messages.EMFContainedHierarchicalCollectionEditionComposite_NewChild;
			case REMOVEELEMENT_BUTTON_ID: return Messages.EMFContainedHierarchicalCollectionEditionComposite_Remove;
			case EXPAND_BUTTON_ID: return Messages.EMFContainedHierarchicalCollectionEditionComposite_Expand;
			case COLLAPSE_BUTTON_ID: return Messages.EMFContainedHierarchicalCollectionEditionComposite_Collapse;
			case UP_BUTTON_ID: return Messages.EMFContainedCollectionEditionComposite_Up;
			case DOWN_BUTTON_ID: return Messages.EMFContainedCollectionEditionComposite_Down;
			default: return null;
		}

	}

	
	protected void createWidgets(FormToolkit toolkit) {
		
		this.setLayout(new GridLayout(3, false));
		
		if(canFilter()) {
			searchLabel = toolkit.createLabel(this, Messages.EMFContainedHierarchicalCollectionEditionComposite_Filter);
			GridData gd = new GridData(GridData.BEGINNING);
			searchLabel.setLayoutData(gd);
			
			searchText = toolkit.createText(this, ""); //$NON-NLS-1$
			GridData gd2 = new GridData(GridData.FILL_HORIZONTAL);
			searchText.setLayoutData(gd2);
			searchText.addListener(SWT.Modify, new Listener() {

				public void handleEvent(Event e) {
					updateFilters();
				}
			});
		}
		
		String[] columnNames = getColumnNames();
		Tree tree = null;
		if((columnNames == null)||(columnNames.length==0))
			tree = toolkit.createTree(this, SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
		else{
			// Multicolumn support
			tree = new Tree(this,SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE |  SWT.H_SCROLL | SWT.V_SCROLL);
			for(int i=0;i<columnNames.length;i++){
				TreeColumn column = new TreeColumn(tree,SWT.SINGLE);
				column.setText(columnNames[i]);
				column.setWidth(90);
				customizeColumn(column);
			}
		}
		GridData gdTree = new GridData(GridData.FILL_BOTH | GridData.HORIZONTAL_ALIGN_BEGINNING);
		if(canFilter()) gdTree.horizontalSpan = 2;

		tree.setLayoutData(gdTree);

		treeViewer = new TreeViewer(tree);
		
		//getTreeViewer().setColumnProperties(columnNames);
		
		getViewer().setCellEditors(getCellEditors(tree));
		getViewer().setCellModifier(getCellModifier(getPage()));
		
		if (getFilter() != null){
			updateFilters();
		}	
		
		tree.setHeaderVisible(true);
		tree.setLinesVisible(true);
		
				
		GridData gdButtonsContainer = new GridData();
		gdButtonsContainer.verticalAlignment = SWT.TOP;
		
		buttonsContainer = toolkit.createComposite(this);
		buttonsContainer.setLayoutData(gdButtonsContainer);
		buttonsContainer.setLayout(new GridLayout(1, true));
		
		
		createAddAndRemoveButtons(buttonsContainer, toolkit);
		createExpandAndCollapseButtons(buttonsContainer, toolkit);
		if(enableOrdering()) {
			createOrderingButtons(buttonsContainer, toolkit);
		}
		
		toolkit.adapt(this);
		
	}
	
	/**
	 * This method lets the descendants to customize every created TableColumn when more than one column is used for table
	 * @param c
	 */
	protected void customizeColumn(TreeColumn c){
		
	}
	
	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#bindFeatureToWidget()
	 */
	@Override
	protected void bindFeatureToWidget() {
		if(getPage().getEditor().getModel() == null ||
				getPage().getEditor().getEditingDomain() == null) return;
		
		IObservableValue selectionObservable = null;
		if (! isDetailComposite()) {
			modelObservable = EMFEditObservables.observeList(getPage().getEditor().getEditingDomain(),this.getEObject(),this.getFeature());			
			
		} else {
			selectionObservable = ViewersObservables
			.observeSingleSelection(this.getMasterViewer());
			
			modelObservable = EMFEditObservables.observeDetailList(Realm.getDefault(),
					this.getPage().getEditor().getEditingDomain(), 
					selectionObservable, this.getFeature());

		}		
		
		// XXX
//		EMFObservableTreeFactory fac = new EMFObservableTreeFactory(getReflexiveFeature(), getPage().getEditor().getEditingDomain(), modelObservable);
//		EMFTreeStructuredAdvisor adv = new EMFTreeStructuredAdvisor(getReflexiveFeature());
//		ObservableListTreeContentProvider contentProvider = new ObservableListTreeContentProvider(fac, adv);
//		getViewer().setContentProvider(contentProvider);
		
		// Hey!!, pay attention to this block. For some reason it is required to pass a different input object instance to AbstractTreeViewer for a 
		// normal mode or master/detail mode. If we pass always the already resolved modelObservable, then the TreeViewer does not work fine.
		if(! isDetailComposite())
			getViewer().setInput(modelObservable);
		else
			getViewer().setInput(selectionObservable);
		
		
		if (getLabelProvider()!=null){
			getViewer().setLabelProvider(getLabelProvider());
		}
	}	
	
	
	@Override
	public void dispose() {
		super.dispose();
		
		if(modelObservable != null) modelObservable.dispose();
	}

	protected boolean canFilter() {
		return false;
	}
	
	protected boolean enableOrdering() {
		return false;
	}
	
	protected CellEditor[] getCellEditors(Tree tree){
		int ncolumns = 1;//getColumNames().length;
		
		CellEditor[] editors = new CellEditor[ncolumns];
		
		for (int i=0; i<ncolumns; i++){
			editors[i] = new TextCellEditor(tree);
		}
		return editors;		
	}
	
	protected Object getNewRootElement() {
		Object newElement = null;
		if(getFeature().getEType() instanceof EClass) {
			EClass eclass = (EClass)getFeature().getEType();
			EObject container = eclass.eContainer();
			if(container instanceof EPackage) {
				EPackage epackage = (EPackage) container;
				EFactory factory = epackage.getEFactoryInstance();
				if(factory != null) {
					if(!eclass.isAbstract()) {
						newElement = factory.create(eclass);
					}
				}
			}
		}
		
		return newElement;
	}
	
	protected Object getNewChildElement() {
		Object newElement = null;
		if(getReflexiveFeature().getEType() instanceof EClass) {
			EClass eclass = (EClass)getReflexiveFeature().getEType();
			EObject container = eclass.eContainer();
			if(container instanceof EPackage) {
				EPackage epackage = (EPackage) container;
				EFactory factory = epackage.getEFactoryInstance();
				if(factory != null) {
					if(!eclass.isAbstract()) {
						newElement = factory.create(eclass);
					}
				}
			}
		}
		
		return newElement;
	}
	
	
	
	protected void updateFilters(){
		List<ViewerFilter> filters = new ArrayList<ViewerFilter>();
		ViewerFilter filter;
		
//		ViewerFilter filter = this.getSearchFilter();
//		if (this.canFilter() && filter != null){
//			filters.add(filter);
//		}
		
		filter = this.getFilter();
		if (filter!=null){
			filters.add(filter);
		}
		
		ViewerFilter[] filtersArray = new ViewerFilter[filters.size()];
		for (int i=0; i<filters.size(); i++){
			filtersArray[i] = filters.get(i);
		}
		
		this.getViewer().setFilters(filtersArray);
	}
	
	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#getLabelText()
	 */
	@Override
	protected String getLabelText() {
		// TODO Auto-generated method stub
		return null;
	}

	
	/* (non-Javadoc)
	 * @see es.cv.gvcase.fefem.common.composites.EMFPropertyComposite#getTargetObservable()
	 */
	@Override
	protected IObservableValue getTargetObservable() {
		// Returns current table seletion observable value
		return ViewersObservables.observeSingleSelection(this.getViewer());
	}
	
	protected String[] getColumnNames(){
		return null;
	}
	
	public EStructuralFeature getRootFeature(){
		return getFeature();
	}
	
	private boolean isChild(EObject eobj) {
		if(getReflexiveFeature() != null && eobj.eContainmentFeature() != null
				&& eobj.eContainmentFeature().equals(getReflexiveFeature())) {
			return true;
		}
		return false;
	}

	protected abstract EStructuralFeature getReflexiveFeature();
	
	protected abstract CellLabelProvider getLabelProvider();

	protected abstract ICellModifier getCellModifier(FEFEMPage page);

}
