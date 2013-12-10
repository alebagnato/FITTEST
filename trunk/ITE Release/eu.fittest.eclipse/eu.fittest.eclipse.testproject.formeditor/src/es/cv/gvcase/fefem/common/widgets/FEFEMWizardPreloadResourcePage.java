package es.cv.gvcase.fefem.common.widgets;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.emf.common.ui.dialogs.WorkspaceResourceDialog;
import org.eclipse.emf.common.util.URI;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.internal.ide.StringMatcher;

import es.cv.gvcase.fefem.action.loadresource.ResourceFilePattern;
import es.cv.gvcase.fefem.common.filters.FileExtensionResourceFilter;
import es.cv.gvcase.fefem.common.internal.Messages;

@SuppressWarnings("restriction")
public class FEFEMWizardPreloadResourcePage extends WizardPage {

	protected ResourceFilePattern[] resourcePatterns;
	protected FileExtensionResourceFilter filter = new FileExtensionResourceFilter();
	protected List<String> resourceURIs = new ArrayList<String>(); 
	protected IWizardPage previousPage;
	
	protected TableViewer tableViewerCandidates;
	
	public FEFEMWizardPreloadResourcePage(String pageName, ResourceFilePattern[] resourcePatterns) {
		super(pageName);
		
		if(resourcePatterns == null)
			resourcePatterns = new ResourceFilePattern[]{new ResourceFilePattern("*.*","All resources")};
		else
			this.resourcePatterns = resourcePatterns;
		
		// By default, the first time the dialog opens all the registered resource patterns are selected 
		for(int i=0;i<resourcePatterns.length;i++)
			filter.registerExtensionPattern(resourcePatterns[i].getPattern());
		
	}

	public ResourceFilePattern[] getResourcePatterns() {
		return resourcePatterns;
	}

	public void setResourcePatterns(ResourceFilePattern[] resourcePatterns) {
		this.resourcePatterns = resourcePatterns;
	}
	
	
	public void createControl(Composite parent) {
		
		// top level group
		Composite topLevel = new Composite(parent, SWT.NONE);
		topLevel.setLayout(new GridLayout());
		topLevel.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING
				| GridData.HORIZONTAL_ALIGN_FILL));
		
		// Table of Resource Pattern filter
		Group container = new Group(topLevel, SWT.BORDER);
		container.setText(Messages.FEFEMWizardPreloadResourcePage_ResourcePatterns);
		FillLayout containerLayout = new FillLayout();
		containerLayout.type = SWT.VERTICAL;
		containerLayout.marginHeight = 5;
		containerLayout.marginWidth = 5;
		containerLayout.spacing = 5;
		container.setLayout(containerLayout);
		container.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Table table = new Table(container, SWT.CHECK | SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI );
			
		String [] columnNames = { Messages.FEFEMWizardPreloadResourcePage_ResourcePattern, Messages.FEFEMWizardPreloadResourcePage_ResourcePatternDescription };

		TableColumn tc0 = new TableColumn(table,SWT.LEFT);
		tc0.setText(columnNames[0]);
		tc0.setWidth(100);
		TableColumn tc1 = new TableColumn(table,SWT.LEFT);
		tc1.setText(columnNames[1]);
		tc1.setWidth(140);
		
		CheckboxTableViewer tableViewer = new CheckboxTableViewer(table);
		tableViewer.setContentProvider(new ArrayContentProvider());
		tableViewer.setInput(resourcePatterns);
		tableViewer.setColumnProperties(columnNames);
		tableViewer.setLabelProvider(new CellLabelProvider(){

			@Override
			public void update(ViewerCell cell) {
				
				if(cell.getColumnIndex() == 0)
					cell.setText(((ResourceFilePattern) cell.getElement()).getPattern());
				else if(cell.getColumnIndex() == 1)
							cell.setText(((ResourceFilePattern) cell.getElement()).getDescription());
				
			}
			
		});
		
		tableViewer.addCheckStateListener(new ICheckStateListener()
		{
		           public void checkStateChanged(CheckStateChangedEvent e)
		           {
		        	   ResourceFilePattern r = (ResourceFilePattern) e.getElement();
		        	   
		        	   if(! filter.isRegisteredExtensionPattern(r.getPattern())){
							filter.registerExtensionPattern(r.getPattern());
						}else
							filter.unregisterExtensionPattern(r.getPattern());
		           }
		});
		
		tableViewer.setAllChecked(true);
		
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		
		
		// Resources candidates to be loaded
		
		Group containerCandidates = new Group(topLevel, SWT.BORDER);
		containerCandidates.setText("Resources to load");
		GridLayout containerCandidatesLayout = new GridLayout();
		containerCandidatesLayout.numColumns=2;
		containerCandidatesLayout.makeColumnsEqualWidth = false;
		containerCandidatesLayout.marginHeight = 5;
		containerCandidatesLayout.marginWidth = 5;
		containerCandidates.setLayout(containerCandidatesLayout);
		containerCandidates.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		Table tableCandidates = new Table(containerCandidates, SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE );
		tableCandidates.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.HORIZONTAL_ALIGN_BEGINNING));
			
		String [] columnNamesCandidates = {Messages.FEFEMWizardPreloadResourcePage_ResourceURIColumn};

		TableColumn tcc0 = new TableColumn(tableCandidates,SWT.LEFT);
		tcc0.setText(columnNamesCandidates[0]);
		tcc0.setWidth(100);

		
		tableViewerCandidates = new TableViewer(tableCandidates);
		tableViewerCandidates.setContentProvider(new ArrayContentProvider());
		tableViewerCandidates.setInput(resourceURIs);
		tableViewerCandidates.setColumnProperties(columnNamesCandidates);
		tableViewerCandidates.setLabelProvider(new CellLabelProvider(){

			@Override
			public void update(ViewerCell cell) {
				
				if(cell.getColumnIndex() == 0)
					cell.setText(((String) cell.getElement()));
			}
			
		});
		
		tableCandidates.setHeaderVisible(true);
		tableCandidates.setLinesVisible(true);
		
		// Buttons for browsing resources
		// 
		Composite buttons = new Composite(containerCandidates, SWT.NONE);
		GridLayout buttonsLayout = new GridLayout();
		buttonsLayout.makeColumnsEqualWidth = true;
		buttonsLayout.numColumns = 1;
		buttons.setLayout(buttonsLayout);
		buttons.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL));
		
		Button bwk = new Button(buttons, SWT.BORDER);
		prepareBrowseWorkspaceButton(bwk);
		bwk.setText(Messages.FEFEMWizardPreloadResourcePage_AddWorkspace);
		GridData bwkGD = new GridData(GridData.FILL_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
		bwk.setLayoutData(bwkGD);
		
		Button bfs = new Button(buttons, SWT.BORDER);
		prepareBrowseFileSystemButton(bfs);
		bfs.setText(Messages.FEFEMWizardPreloadResourcePage_AddFilesystem);
		bfs.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));
		
		// Button for remove candidates
		Button bCandidatesRemove = new Button(buttons, SWT.BORDER);
		bCandidatesRemove.setText(Messages.FEFEMWizardPreloadResourcePage_Remove);
		bCandidatesRemove.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL ));
		bCandidatesRemove.addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				IStructuredSelection selection = (IStructuredSelection) tableViewerCandidates.getSelection();
				if(selection != null){
					resourceURIs.remove(selection.getFirstElement());
					tableViewerCandidates.refresh();
					updatePageStatus();
				}
			}
			
		});
		
		// Show description on opening
		setErrorMessage(null);
		setMessage(null);
		setControl(topLevel);
		
		updatePageStatus();
	}

	public String[] getResourceURIs() {
		return resourceURIs.toArray(new String[0]);
	}

	protected void prepareBrowseWorkspaceButton(Button browseWorkspaceButton) {
		
		browseWorkspaceButton.addSelectionListener
	      (new SelectionAdapter()
	       {
	         @Override
	        public void widgetSelected(SelectionEvent event)
	         {
	           
	           // ResourcePattern string message
	           String msg = Messages.FEFEMWizardPreloadResourcePage_ResourcePatterns + ": ";
	           String[] rPatterns = filter.getRegisteredExtensionPatterns();
	           if(rPatterns == null)
	        	   msg = msg + "*.*";
	           else{
	        	   msg = msg + "[";
	        	   for(int i=0;i<rPatterns.length;i++){
	        		   if(i == rPatterns.length -1)
	        			  msg = msg + rPatterns[i];
	        		  else
	        			  msg = msg + rPatterns[i] + ";";
	        	   }
	        	   msg = msg + "]";
	           }
	           
	           List<ViewerFilter> viewerFilters = new ArrayList<ViewerFilter>();
	           viewerFilters.add(filter);

	             IFile[] files = WorkspaceResourceDialog.openFileSelection(getShell(), null, msg, true, null, viewerFilters);
	             for (int i = 0, len = files.length; i < len; i++)
	             {
	            	 resourceURIs.add(URI.createPlatformResourceURI(files[i].getFullPath().toString(), false).toString());
	            	 tableViewerCandidates.refresh();
	            	 updatePageStatus();
	             }

	         }
	       });      
		
	}

	protected void prepareBrowseFileSystemButton(Button browseFileSystemButton) {
		
		browseFileSystemButton.addSelectionListener(new SelectionAdapter()  {
			
		            public void widgetSelected(SelectionEvent event)
		            {
		              FileDialog fileDialog = new FileDialog(getShell());
		              if(filter != null){
		            	  fileDialog.setFilterExtensions(filter.getRegisteredExtensionPatterns());
		              }
		              fileDialog.open();
		              
		              
		              if (fileDialog.getFileName() != null && fileDialog.getFileName().length() > 0)
		              {
		                String filePath = fileDialog.getFilterPath() + File.separator + fileDialog.getFileName();
		                resourceURIs.add((URI.createFileURI(filePath).toString()).trim());
		                tableViewerCandidates.refresh();
		                updatePageStatus();
		              }
		            }
		            
		 });
	}

	protected void updatePageStatus(){
		if(getWizard().getContainer().getCurrentPage() != null) 
			getWizard().getContainer().updateButtons();
		if(!validatePage())
			this.setMessage(Messages.FEFEMWizardPreloadResourcePage_NoResourcesMessage , DialogPage.ERROR);
		else
			this.setMessage(Messages.FEFEMWizardPreloadResourcePage_ValidPageMessage , DialogPage.NONE);
	}
	
	@SuppressWarnings("restriction")
	public boolean validatePage(){
		
		// Verify there are resource candidates that matches all resource file registered patterns .
		
		boolean r = true;
		
		for(int i=0;i<resourcePatterns.length;i++){
			
			boolean existsResource = false;
			for(int z=0;z<resourceURIs.size();z++){
				existsResource = existsResource || (new StringMatcher(resourcePatterns[i].getPattern(),false,false).match(resourceURIs.get(z)));
			}
			
			r = r && existsResource;
		}
		
		return r;
	}
	
	@Override
	public IWizardPage getPreviousPage() {
		return (IWizardPage) previousPage;
	}

	public void setPreviousPage(IWizardPage previousPage) {
		this.previousPage = previousPage;
	}

	@Override
	public boolean isPageComplete() {
		return validatePage();
	}

	@Override
	public void dispose() {
		
		getControl().dispose();
		
		super.dispose();
	}


	
	
}
