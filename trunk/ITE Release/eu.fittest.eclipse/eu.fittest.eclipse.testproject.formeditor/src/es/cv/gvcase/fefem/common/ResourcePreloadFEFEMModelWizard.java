/***************************************************************************
* Copyright (c) 2008 Conselleria de Infraestructuras y Transporte,
* Generalitat de la Comunitat Valenciana . All rights reserved. This program
* and the accompanying materials are made available under the terms of the
* Eclipse Public License v1.0 which accompanies this distribution, and is
* available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors: Jose Manuel Garc��a Valladolid (CIT) - Initial API and implementation
*
**************************************************************************/

package es.cv.gvcase.fefem.common;


import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.WorkspaceModifyOperation;

import es.cv.gvcase.fefem.action.loadresource.ResourceFilePattern;
import es.cv.gvcase.fefem.common.widgets.FEFEMWizardPreloadResourcePage;


/**
 * This is an extended FEFEMModelWizard that provides an aditional page for loading required
 * resources to the EditingDomain
 * 
 * @author Jose Manuel Garc��a Valladolid
 */
public abstract class ResourcePreloadFEFEMModelWizard extends FEFEMModelWizard {

	protected FEFEMWizardPreloadResourcePage resPage;
	
	@Override
	public void addPages() {
				
		super.addPages();
				
		ResourceFilePattern[] rfp = getResourceFilePatterns();
		if(rfp == null)
			rfp = new ResourceFilePattern[]{new ResourceFilePattern("*.*","All resources")};
		resPage = new FEFEMWizardPreloadResourcePage("Whatever", rfp);
		resPage.setTitle(getEditorPlugin().getString("_UI_"+this.getModelName()+"ModelWizard_label"));
		resPage.setDescription(getEditorPlugin().getString("_UI_"+this.getModelName()+"ModelWizard_description"));
		resPage.setPreviousPage(this.newFileCreationPage);
		addPage(resPage);
		
	}

	@Override
	public boolean performFinish() {
			
		boolean superResult = super.performFinish();
		if(superResult){
			
			WorkspaceModifyOperation operation =
				new WorkspaceModifyOperation() {
					@Override
					protected void execute(IProgressMonitor progressMonitor) {
						try {
							IWorkbenchWindow workbenchWindow = workbench.getActiveWorkbenchWindow();
							IWorkbenchPage page = workbenchWindow.getActivePage();
							FEFEMEditor editor = (FEFEMEditor) page.getActiveEditor();
							
							if(resPage.getResourceURIs() != null){
								for(int i=0;i<resPage.getResourceURIs().length;i++){
									  editor.getEditingDomain().loadResource(resPage.getResourceURIs()[i]);
								}
							}
							
							initializeEditor(editor);
						}
						catch (Exception exception) {
							getEditorPlugin().log(exception);
						}
						finally {
							progressMonitor.done();
						}
					}
					
				};

			try {
				getContainer().run(false, false, operation);
				return true;
			} catch (Exception e) {
				getEditorPlugin().log(e);
				return false;
			}
			

		}
		
		return superResult;
	}

	
	@Override
	public boolean canFinish() {
		return (this.newFileCreationPage.canFlipToNextPage() && resPage.validatePage());
	}
 
	
	@Override 
	public boolean isHelpAvailable() {
		return false;
	}

	@Override
	public void dispose() {
		
		resPage.dispose();
		
		super.dispose();
	}

	/**
	 * This method is intented to be overrided by subclasses in order to execute any editor initialization code
	 * @param editor
	 */
	protected void initializeEditor(FEFEMEditor editor){
		
	}
	
	protected abstract ResourceFilePattern[] getResourceFilePatterns();
}
