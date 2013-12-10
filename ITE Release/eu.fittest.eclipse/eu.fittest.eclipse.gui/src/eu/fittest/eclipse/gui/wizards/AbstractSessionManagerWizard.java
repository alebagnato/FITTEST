package eu.fittest.eclipse.gui.wizards;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;
import eu.fittest.project.config.SUTTechnologyType;
import eu.fittest.project.config.TestProject;

public abstract class AbstractSessionManagerWizard extends Wizard implements INewWizard {
	protected List<IFITTESTComponentManager> componentManagers;
	protected IProject selectedProject;
	protected AbstractSessionWizardPageOne pageOne;
	
	TestProject projectConfig;
	
	protected AbstractSessionManagerWizard(String wizardName) {
		this.componentManagers = new ArrayList<IFITTESTComponentManager>();
		setWindowTitle(wizardName);
		
		projectConfig = Activator.getDefault().getActiveProjectConfig();
	}
	
	public void init(IWorkbench workbench, IProject project) {
		this.selectedProject = project;
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		if(selection.getFirstElement()!=null){
			this.selectedProject = ((IResource)selection.getFirstElement()).getProject();
		}
	}
	
	protected void addFITTESTComponentPages(SessionType sessionType) {
		IConfigurationElement[] config = Platform.getExtensionRegistry().getConfigurationElementsFor(IFITTESTComponentManager.TESTING_SESSION_EXTENSION_POINT_ID);
			for (IConfigurationElement element : config) {
				if(element.getAttribute("sessionType").equals(sessionType.getName())){
					try {
						Object o = element.createExecutableExtension("class");
						if (o instanceof IFITTESTComponentManager) {
							IFITTESTComponentManager cm = (IFITTESTComponentManager)o;
							
							boolean isComponentNeeded = false;
							for (SUTTechnologyType type :  cm.getSupportSUT()){
								if (projectConfig.getGeneral().getType().equals(type)){
									isComponentNeeded = true;
									break;
								}
							}
							if (isComponentNeeded) {
								this.componentManagers.add(cm);
								for(IFITTESTComponentWizardPage page : cm.getConfigurationPages()){
									addPage(page);
								}
							}
						}
					} catch (CoreException e) {
						Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
					}
				}
			}
	}
	
	@Override
	public boolean performFinish() {
		boolean result = false;
		IFolder session = this.selectedProject.getFolder(IFITTESTFolderConstants.TESTING_SESSIONS + "/" + pageOne.getSessionName());

		// assign this folder to be the current log folder
		projectConfig.getLogging().getLogTarget().setStoreDir(session.getFullPath().toOSString());
		
		try {
			if(!session.exists()) session.create(false, true, null);
			FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(IFileTransferService.class).setBasePath(session.getLocationURI().toString()+"/");
			List<Host> HUTs = new ArrayList<Host>();
			for(Host h : this.pageOne.getHosts()){
				if(h.isHUT()){
					HUTs.add(h);
				}
			}
			
			createJob(session, HUTs);
			result = true;
		} catch (Exception e) {
			e.printStackTrace();
			MessageDialog.openError(getShell(), this.pageOne.getName() + " error", e.getMessage());
		}
		return result;
	}

	abstract protected void createJob(IFolder session, List<Host> hUTs)  throws FITTESTException;

}
