package eu.fittest.eclipse.gui.wizards.project;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;













import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;

import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.nature.TestingProject;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.gui.utils.Viewer;
import eu.fittest.eclipse.gui.views.job.JobsView;
import eu.fittest.eclipse.gui.wizards.FITTESTWizardMessages;
import eu.fittest.project.ProjectConfigUtils;
import eu.fittest.project.config.SUTTechnologyType;
import eu.fittest.project.config.TestProject;

public class FITTESTProjectWizard extends Wizard implements INewWizard,
		IExecutableExtension {
	private static final String WIZARD_NAME = FITTESTWizardMessages.FITTESTProject_NewTestingProjectWizardName;

	private IConfigurationElement configurationElement;
	private FITTESTProjectWizardPageOne pageOne;
	private IWorkbench workbench;

	public FITTESTProjectWizard() {
		setWindowTitle(WIZARD_NAME);
	}
	
	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
	}

	@Override
	public void addPages() {
		pageOne = new FITTESTProjectWizardPageOne();
		addPage(pageOne);
	}

	@Override
	public boolean performFinish() {
		boolean result = true;
		try {
			IProject project = createBaseProject(pageOne.getProjectName());
			addNature(project, TestingProject.NATURE_ID);
			addNature(project, JavaCore.NATURE_ID);

			String[] paths = {/*
							 * PRODUCTION_SERVERS, PRODUCTION_CLIENTS,
							 * TEST_SERVERS,TEST_CLIENTS,
							 */IFITTESTFolderConstants.MODELS,
					IFITTESTFolderConstants.TEST_SUITES,
					IFITTESTFolderConstants.TESTING_SESSIONS }; //$NON-NLS-1$ //$NON-NLS-2$
			addToProjectStructure(project, paths);
			addProjectConfiguration(project, pageOne.getProjectType(), pageOne.getProjectBaseURL(),
					pageOne.getProjectEntryPage(), pageOne.getProjectServerFolder());

			IJavaProject javaProject = JavaCore.create(project);

			List<IClasspathEntry> entries = new ArrayList<IClasspathEntry>();
			entries.add(JavaRuntime.getDefaultJREContainerEntry());

			IPath path = new Path("org.eclipse.jdt.USER_LIBRARY/FITTEST_libs");
			entries.add(JavaCore.newContainerEntry(path));

			IPackageFragmentRoot sourceRoot = javaProject
					.getPackageFragmentRoot(project
							.getFolder(IFITTESTFolderConstants.TEST_SUITES));
			entries.add(JavaCore.newSourceEntry(sourceRoot.getPath()));

			javaProject.setRawClasspath(
					entries.toArray(new IClasspathEntry[entries.size()]), null);

			BasicNewProjectResourceWizard
					.updatePerspective(configurationElement);
			
			// show the view
			workbench.getActiveWorkbenchWindow().getActivePage().showView("eu.fittest.eclipse.allinonegui.views.AllInOneView");

			// open the editor for the configuration file
			project.refreshLocal(IFolder.DEPTH_INFINITE, null);
			
			IFile projectConfigFile = project.getFile("fittest.project");
			if (projectConfigFile.exists()){
				IEditorDescriptor[] descs =  PlatformUI.getWorkbench().getEditorRegistry().getEditors(projectConfigFile.getName()); //PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(projectConfigFile.getName());
				for (IEditorDescriptor editorDesc : descs){
					if (editorDesc.getId().equals("eu.fittest.eclipse.testproject.formeditor.ProjectEditor")){
						workbench.getActiveWorkbenchWindow().getActivePage().openEditor(new FileEditorInput(projectConfigFile), editorDesc.getId());
						return true;
					}
				}
				// open with a default editor
				IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(projectConfigFile.getName());
				if (desc != null)
					workbench.getActiveWorkbenchWindow().getActivePage().openEditor(new FileEditorInput(projectConfigFile), desc.getId());
				
			}
			
			
		} catch (CoreException e) {
			e.printStackTrace();
			result = false;
		
		}
			
		return result;
	}

	/**
	 * 
	 * Add project configuration file by instantiate a copy
	 * from the template that come with the FITTEST GUI plugin
	 * 
	 * @author cdnguyen
	 * @param project
	 * @param projectType
	 * @param projectBaseURL
	 * @param projectEntryPage
	 * @param projectServerFolder
	 */
	private void addProjectConfiguration(IProject project, String projectType,
			String projectBaseURL, String projectEntryPage,
			String projectServerFolder) {
		
		IPath path =  new Path("resources" + File.separator + "templates" + File.separator + "TestProject.xml");
		URL projectConfigTemplateFile = Platform.getBundle(Activator.PLUGIN_ID).getEntry(path.toOSString());
		
		File f;
		try {
			f = new File(FileLocator.toFileURL(projectConfigTemplateFile).getFile());
			if (f.exists()){
				
				try {
					TestProject projectConfigInstance = ProjectConfigUtils.load(f);
					
					projectConfigInstance.getGeneral().setBaseURL(projectBaseURL);
					projectConfigInstance.getGeneral().setEntryPage(projectEntryPage);
					projectConfigInstance.getGeneral().setType(SUTTechnologyType.valueOf(projectType));
					if (!projectServerFolder.isEmpty())
						projectConfigInstance.getGeneral().setServerFolder(projectServerFolder);
					
					String saveTo = project.getLocation().append("fittest.project").toOSString();
					ProjectConfigUtils.save(projectConfigInstance, saveTo);
					
					return;
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			} 
			
			if (workbench != null) {
				workbench.getDisplay().asyncExec(new Runnable() {
					public void run() {
						MessageDialog.openWarning(workbench.getActiveWorkbenchWindow().getShell(), "Project Creation", 
								"Something went wrong while creating the project configuration file.\n"
										+ "Please check if the template file exists and is valid!");
					}
				});
			}
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}

	private static IProject createBaseProject(String projectName) {
		IProject newProject = ResourcesPlugin.getWorkspace().getRoot()
				.getProject(projectName);
		if (!newProject.exists()) {
			IProjectDescription desc = newProject.getWorkspace()
					.newProjectDescription(newProject.getName());
			desc.setLocationURI(null);
			try {
				newProject.create(desc, null);
				if (!newProject.isOpen()) {
					newProject.open(null);
				}
			} catch (CoreException e) {
				e.printStackTrace();
			}
		}
		return newProject;
	}

	private static void addNature(IProject project, String nature)
			throws CoreException {
		if (!project.hasNature(nature)) {
			IProjectDescription description = project.getDescription();
			String[] prevNatures = description.getNatureIds();
			String[] newNatures = new String[prevNatures.length + 1];
			System.arraycopy(prevNatures, 0, newNatures, 0, prevNatures.length);
			newNatures[prevNatures.length] = nature;
			description.setNatureIds(newNatures);

			IProgressMonitor monitor = null;
			project.setDescription(description, monitor);
		}
	}

	/**
	 * Create a folder structure with a parent root, overlay, and a few child
	 * folders.
	 * 
	 * @param newProject
	 * @param paths
	 * @throws CoreException
	 */
	private static void addToProjectStructure(IProject newProject,
			String[] paths) throws CoreException {
		for (String path : paths) {
			IFolder etcFolders = newProject.getFolder(path);
			ResourceUtils.createFolder(etcFolders);
		}
	}

	@Override
	public void setInitializationData(IConfigurationElement config,
			String propertyName, Object data) throws CoreException {
		configurationElement = config;
	}

}
