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

package es.cv.gvcase.fefem.common.widgets;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.emf.common.ui.dialogs.WorkspaceResourceDialog;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.edit.ui.action.LoadResourceAction.LoadResourceDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import es.cv.gvcase.fefem.action.loadresource.ResourceFilePattern;
import es.cv.gvcase.fefem.common.filters.FileExtensionResourceFilter;
import es.cv.gvcase.fefem.common.internal.Messages;

/**
 * @author Jose Manuel García Valladolid
 */
public class FilteredLoadResourceDialog extends LoadResourceDialog {

	protected ResourceFilePattern[] resourcePatterns;
	protected FileExtensionResourceFilter filter = new FileExtensionResourceFilter();
		
	/**
	 * @param arg0
	 */
	public FilteredLoadResourceDialog(Shell arg0) {
		super(arg0);
	}

	/**
	 * @param arg0
	 * @param arg1
	 */
	public FilteredLoadResourceDialog(Shell arg0, EditingDomain arg1) {
		super(arg0, arg1);
	}

	
	
	public ResourceFilePattern[] getResourcePatterns() {
		return resourcePatterns;
	}

	public void setResourcePatterns(ResourceFilePattern[] resourcePatterns) {
		this.resourcePatterns = resourcePatterns;
		
		if(resourcePatterns != null){
			
	    }
	}

	@Override
	public int open() {
		
		// If no patterns were specified, then create a non restrictive one like '*.*'
		if(resourcePatterns == null)
			resourcePatterns = new ResourceFilePattern[]{new ResourceFilePattern("*.*","All resources")};
		
		// By default, the first time the dialog opens all the registered resource patterns are selected 
		for(int i=0;i<resourcePatterns.length;i++)
			filter.registerExtensionPattern(resourcePatterns[i].getPattern());
		
		return super.open();
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		
		// If the EditingDomain is available then the dialog shows current loaded resources
		if(this.domain != null){
			
			Group resContainer = new Group(parent, SWT.BORDER);
			resContainer.setText(Messages.FilteredLoadResourceDialog_LoadedResources);
			FillLayout rescontainerLayout = new FillLayout();
			rescontainerLayout.type = SWT.VERTICAL;
			rescontainerLayout.marginHeight = 5;
			rescontainerLayout.marginWidth = 5;
			rescontainerLayout.spacing = 5;
			resContainer.setLayout(rescontainerLayout);
			resContainer.setLayoutData(new GridData(GridData.FILL_BOTH));
			
			Table table = new Table(resContainer, SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
				
			String [] columnNames = { Messages.FilteredLoadResourceDialog_ResourceLocation};
					
			TableViewer tableViewer = new TableViewer(table);
			tableViewer.setContentProvider(new ArrayContentProvider());
			tableViewer.setInput(this.domain.getResourceSet().getResources());
			tableViewer.setColumnProperties(columnNames);
			tableViewer.setLabelProvider(new CellLabelProvider(){

				@Override
				public void update(ViewerCell cell) {
					
					cell.setText(((Resource) cell.getElement()).getURI().toString());
					
					
				}
				
			});
			
			TableColumn tc0 = new TableColumn(table,SWT.LEFT);
			tc0.setText(columnNames[0]);
			tc0.setWidth(150);
						
			table.setHeaderVisible(true);
			table.setLinesVisible(true);
		}
		
		
		Group container = new Group(parent, SWT.BORDER);
		container.setText(Messages.FilteredLoadResourceDialog_ResourcePatterns);
		FillLayout containerLayout = new FillLayout();
		containerLayout.type = SWT.VERTICAL;
		containerLayout.marginHeight = 5;
		containerLayout.marginWidth = 5;
		containerLayout.spacing = 5;
		container.setLayout(containerLayout);
		container.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		Table table = new Table(container, SWT.CHECK | SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI );
			
		String [] columnNames = { Messages.FilteredLoadResourceDialog_ResourcePattern, Messages.FilteredLoadResourceDialog_ResourcePatternDescription };

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
		
		
		return super.createDialogArea(parent);
	}

	@Override
	protected void prepareBrowseWorkspaceButton(Button browseWorkspaceButton) {
		
		browseWorkspaceButton.addSelectionListener
	      (new SelectionAdapter()
	       {
	         @Override
	        public void widgetSelected(SelectionEvent event)
	         {
	           
	           // ResourcePattern string message
	           String msg = Messages.FilteredLoadResourceDialog_ResourcePatterns + ": ";
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
	           
            	 if (isMulti())
	           {
	             StringBuffer uris = new StringBuffer();

	             IFile[] files = WorkspaceResourceDialog.openFileSelection(getShell(), null, msg, true, null, viewerFilters);
	             for (int i = 0, len = files.length; i < len; i++)
	             {
	               uris.append(URI.createPlatformResourceURI(files[i].getFullPath().toString(), true));
	               uris.append("  ");
	             }
	             uriField.setText((uriField.getText() + "  " + uris.toString()).trim());
	           }
	           else
	           {
	             IFile file = null;

	             if (isSave())
	             {
	               file = WorkspaceResourceDialog.openNewFile(getShell(), null, msg, null, viewerFilters);
	             }
	             else
	             {
	               IFile[] files = WorkspaceResourceDialog.openFileSelection(getShell(), null, msg, false, null, viewerFilters);
	               if (files.length != 0)
	               {
	                 file = files[0];
	               }
	             }
	             
	             if (file != null)
	             {
	               uriField.setText(URI.createPlatformResourceURI(file.getFullPath().toString(), true).toString());
	             }
	           }
	         }
	       });      
		
	}

	@Override
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
		                uriField.setText((uriField.getText() + " " + URI.createFileURI(filePath).toString()).trim());
		              }
		            }
		            
		 });
	}
	
	
}
