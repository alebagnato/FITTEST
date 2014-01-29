/***************************************************************************
* Copyright (c) 2008, 2009 Conselleria de Infraestructuras y Transporte,
* Generalitat de la Comunitat Valenciana, Open Canarias S.L. All rights reserved. 
* This program and the accompanying materials are made available under the terms 
* of the Eclipse Public License v1.0 which accompanies this distribution, and is
* available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors:
* Javier Mu�oz Ferrara (Prodevelop)
* Mario Cervera Ubeda (Prodevelop)
* Adolfo Sanchez-Barbudo Herrera (Open Canarias S.L) - Bug 2340
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
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.command.DeleteCommand;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.ViewerSorter;
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
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.internal.Messages;

public abstract class EMFContainedCollectionEditionComposite extends EMFPropertyComposite {

	protected static final int ADDELEMENT_BUTTON_ID = 0;
	protected static final int REMOVEELEMENT_BUTTON_ID = 1;
	protected static final int UP_BUTTON_ID = 2;
	protected static final int DOWN_BUTTON_ID = 3;
	
	private TableViewer tableViewer;
	
	private Label searchLabel;
	
	private Text searchText;
	
	private Composite buttonsContainer;

	protected List<Button> addButtons;

	protected List<Button> removeButtons;
	
	protected Button upButton;
	
	protected Button downButton;

	protected IObservableList modelObservable;

	public EMFContainedCollectionEditionComposite(Composite parent, int style,
			FormToolkit toolkit, EObject object, FEFEMPage page) {
		super(parent, style, toolkit, object, page);
	}

	public EMFContainedCollectionEditionComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
		this.getMasterViewer().addSelectionChangedListener(new ISelectionChangedListener(){

			public void selectionChanged(SelectionChangedEvent event) {
				updateButtonsEnablement();				
			}
			
		});
	}

	public Text getSearchText() {
		return searchText;
	}
	
	public TableViewer getViewer(){
		return this.tableViewer;
	}
	
	protected ViewerFilter getFilter(){
		return null;
	}
	
	@Override
	protected void createWidgets(FormToolkit toolkit) {
		
		this.setLayout(new GridLayout(3, false));
		
		if(canFilter()) {
			searchLabel = toolkit.createLabel(this, Messages.EMFContainedCollectionEditionComposite_Filter + ':');
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
		
		String[] columnNames = getColumNames();
		
		Table table = toolkit.createTable(this, SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
		GridData gdTable = new GridData(GridData.FILL_BOTH | GridData.HORIZONTAL_ALIGN_BEGINNING);
		gdTable.heightHint = 1;
		if(canFilter()) gdTable.horizontalSpan = 2;
		table.setLayoutData(gdTable);

		tableViewer = new TableViewer(table);
		
		getViewer().setColumnProperties(columnNames);
		
		getViewer().setCellEditors(getCellEditors(table));
		getViewer().setCellModifier(getCellModifier(getPage()));
		
		if (getFilter() != null){
			updateFilters();
		}
		
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		
		createColumns(table);
		
		GridData gdButtonsContainer = new GridData();
		gdButtonsContainer.verticalAlignment = SWT.TOP;
		if(canFilter()) gdButtonsContainer.verticalSpan = 2;
		buttonsContainer = toolkit.createComposite(this);
		buttonsContainer.setLayoutData(gdButtonsContainer);
		buttonsContainer.setLayout(new GridLayout(1, true));
		
		createAddAndRemoveButtons(buttonsContainer, toolkit);
		createAdditionalButtons(buttonsContainer, toolkit);
		if(enableOrdering()) {
			createOrderingButtons(buttonsContainer, toolkit);
		}

		updateButtonsEnablement();
		
		toolkit.adapt(this);

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
		addButtons.add(addButton);		
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
	
	protected void createAddAndRemoveButtons(Composite container,
			FormToolkit toolkit) {
		
		createAddButton(container, getButtonLabel(ADDELEMENT_BUTTON_ID), toolkit);
		
		createRemoveButton(container, getButtonLabel(REMOVEELEMENT_BUTTON_ID), toolkit);
		
	}
	
	protected void createOrderingButtons(Composite container,
			FormToolkit toolkit) {
		
		createUpButton(container, getButtonLabel(UP_BUTTON_ID), toolkit);
		
		createDownButton(container, getButtonLabel(DOWN_BUTTON_ID), toolkit);
		
	}

	protected String getButtonLabel(int buttonID){
		switch (buttonID){
			case ADDELEMENT_BUTTON_ID: return Messages.EMFContainedCollectionEditionComposite_CreateElement;
			case REMOVEELEMENT_BUTTON_ID: return Messages.EMFContainedCollectionEditionComposite_RemoveElement;
			case UP_BUTTON_ID: return Messages.EMFContainedCollectionEditionComposite_Up;
			case DOWN_BUTTON_ID: return Messages.EMFContainedCollectionEditionComposite_Down;
			default: return null;
		}

	}
	
	protected SelectionListener getAddButtonSelectionListener() {		
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				Object newElement = getNewElement();
				
				if(modelObservable != null && newElement != null) {					
					modelObservable.add(newElement);
					getPage().setDirty(true);
					getViewer().setSelection(new StructuredSelection(newElement));
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
				TableViewer viewer = getViewer(); 
				if(viewer.getSelection() instanceof StructuredSelection) {
					StructuredSelection selection = (StructuredSelection) getViewer().getSelection();
					List<?> elementsToDelete = selection.toList();
					if(elementsToDelete.size() > 0) {
						// We prepare the next selection, based on the element to delete
						EObject eObject = (EObject) elementsToDelete.get(0);
						StructuredSelection nextSelection = StructuredSelection.EMPTY;
						if (modelObservable.size() > 1) { // If we have more than one element
							int pos = modelObservable.indexOf(eObject);
							nextSelection = (pos == modelObservable.size() - 1) // If it's the last first level element, we will select the previous sibling
								? new StructuredSelection(modelObservable.get(pos - 1))
								: new StructuredSelection (modelObservable.get(pos + 1));	// otherwise, we will select the next one
						}

						EStructuralFeature feature = getFeature();
						if (feature instanceof EReference	// If it's a containment reference we delete the elements from the model
							&& ((EReference)feature).isContainment()) {
							getEditingDomain().getCommandStack().execute(DeleteCommand.create(getEditingDomain(), elementsToDelete)); 
						} else { 							// otherwise, we just remove the elements from the obsevableList
							modelObservable.removeAll(elementsToDelete);
						}

						getPage().setDirty(true);
						getViewer().setSelection(nextSelection);
					}
				}
			}
		};
		return adapter;
	}

	protected SelectionListener getUpButtonSelectionListener() {
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				if(getViewer().getSelection() instanceof StructuredSelection
						&& modelObservable instanceof IObservableList) {
					Object elementToMove = ((StructuredSelection)getViewer().getSelection()).getFirstElement();
					if(elementToMove != null) {
						int oldIndex = ((IObservableList)modelObservable).indexOf(elementToMove);
						int newIndex = oldIndex == 0? oldIndex : oldIndex - 1;
						if(oldIndex != newIndex) {
							getViewer().setSorter(null);
							((IObservableList)modelObservable).move(oldIndex, newIndex);
							getPage().setDirty(true);
						}
					}
				}
			}

		};
		return adapter;
	}
	
	protected SelectionListener getDownButtonSelectionListener() {
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				if(getViewer().getSelection() instanceof StructuredSelection
						&& modelObservable instanceof IObservableList) {
					Object elementToMove = ((StructuredSelection)getViewer().getSelection()).getFirstElement();
					if(elementToMove != null) {
						int oldIndex = ((IObservableList)modelObservable).indexOf(elementToMove);
						int newIndex = oldIndex == ((IObservableList)modelObservable).size() - 1?
								oldIndex : oldIndex + 1;
						if(oldIndex != newIndex) {
							getViewer().setSorter(null);
							((IObservableList)modelObservable).move(oldIndex, newIndex);
							getPage().setDirty(true);
						}
					}
				}
			}

		};
		return adapter;
	}
	
	protected Object getNewElement() {
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
		}else if(getFeature().getEType() instanceof EDataType){
			EDataType eatt = (EDataType)getFeature().getEType();
			EObject container = eatt.eContainer();
			if(container instanceof EPackage) {
				EPackage epackage = (EPackage) container;
				EFactory factory = epackage.getEFactoryInstance();
				if(factory != null) {
					newElement = factory.createFromString(eatt, "New Item");
				}
			}
		}
		
		return newElement;
	}
	
	protected boolean enableOrdering() {
		return false;
	}
	
	protected void createAdditionalButtons(Composite container,
			FormToolkit toolkit) {
		
	}
	
	@Override
	public void dispose() {
		super.dispose();
		
		if(modelObservable != null) modelObservable.dispose();
	}

	protected void createColumns(Table table) {
		int ncolumns = getColumNames().length;
		
		for (int i=0; i<ncolumns; i++){
			TableColumn column = new TableColumn(table, SWT.LEFT);
			column.setText(getColumNames()[i]);
			column.setWidth(120);
			customizeColumn(column);
			
			//Set a sorter when clicking on the column header
			
			final ViewerSorter sorter = getSorter(getColumNames()[i]);
			
			if (sorter != null){
				column.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						ArrayList<Object> elements = new ArrayList<Object>();
						for(int i = 0; i < getViewer().getTable().getItemCount(); i++) {
							elements.add(getViewer().getElementAt(i));
						}
						
						getViewer().setSorter(sorter);
						
						ArrayList<Object> orderedElements = new ArrayList<Object>();
						for(int i = 0; i < getViewer().getTable().getItemCount(); i++) {
							orderedElements.add(getViewer().getElementAt(i));
						}
						
						if(!elements.equals(orderedElements)) {
							ISelection selection = getViewer().getSelection();
							((IObservableList)modelObservable).clear();
							((IObservableList)modelObservable).addAll(orderedElements);
							getViewer().setSelection(selection);
							getPage().setDirty(true);
						}
					}
	
				});
			}
		
		}		
	}
	
	/**
	 * This method lets the descendants to customize every created TableColumn when more than one column is used for table
	 * @param c
	 */
	protected void customizeColumn(TableColumn c){
		
	}
	
	protected ViewerSorter getSorter(String columnName) {
		return null;
	}
	
	protected boolean canFilter() {
		return false;
	}
	
	protected ViewerFilter getSearchFilter() {
		return null;
	}

	protected CellEditor[] getCellEditors(Table table){
		int ncolumns = getColumNames().length;
		
		CellEditor[] editors = new CellEditor[ncolumns];
		
		for (int i=0; i<ncolumns; i++){
			editors[i] = new TextCellEditor(table);
		}
		return editors;		
	}
	

	@Override	
	protected void bindFeatureToWidget() {
		if(getPage().getEditor().getModel() == null ||
				getPage().getEditor().getEditingDomain() == null) return;
		
		//Observes the containedTerms list to get automatic refresh of the TableViewer.
		//It will also allow to add and remove elements from the list without using commands.
	
		if (! isDetailComposite()) {
			modelObservable = EMFEditObservables.observeList(getPage().getEditor().getEditingDomain(),
					this.getEObject(),
					getFeature());			
			
		} else {
			IObservableValue selectionObservable = ViewersObservables
			.observeSingleSelection(this.getMasterViewer());
			
			modelObservable = EMFEditObservables.observeDetailList(Realm.getDefault(),
					this.getPage().getEditor().getEditingDomain(), 
					selectionObservable, getFeature());

		}


		
		ObservableListContentProvider contentProvider = new ObservableListContentProvider();
		tableViewer.setContentProvider(contentProvider);
		tableViewer.setInput(modelObservable);
		if (getLabelProvider()!=null){
			tableViewer.setLabelProvider(getLabelProvider());
		}
	}
	
	protected void updateFilters(){
		List<ViewerFilter> filters = new ArrayList<ViewerFilter>();
		ViewerFilter filter = this.getSearchFilter();
		if (this.canFilter() && filter != null){
			filters.add(filter);
		}
		
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
	
	
	private void updateButtonsEnablement(){
		if (this.isDetailComposite()){
			boolean enablement = ! getMasterViewer().getSelection().isEmpty(); 
			
			for (Button b : this.addButtons){
				if(!b.isDisposed())
					b.setEnabled(enablement);
			}
			for (Button b : this.removeButtons){
				if(!b.isDisposed())
					b.setEnabled(enablement);
			}						
		}
	}

	protected abstract String[] getColumNames();
	
	protected abstract CellLabelProvider getLabelProvider();
	
	protected abstract ICellModifier getCellModifier(FEFEMPage page);

}
