package eu.fittest.eclipse.gui;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.nature.TestingProject;
import eu.fittest.eclipse.gui.utils.Viewer;
import eu.fittest.eclipse.gui.workspacelisteners.ClosingListener;
import eu.fittest.eclipse.gui.workspacelisteners.ProjectConfigFileModificationListener;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;
import eu.fittest.project.ProjectConfigUtils;
import eu.fittest.project.config.TestProject;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	public static final String IMG_HORIZONTAL = "horizontal";
	public static final String IMG_VERTICAL = "vertical";
	public static final String IMG_FORM_BG = "formBg";

	// The plug-in ID
	public static final String PLUGIN_ID = "eu.fittest.eclipse.gui"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;

	private HashMap<IProject, TestProject> projectConfigs = null;
	private IProject recentSelectedProject = null; 

	/**
	 * The constructor
	 */
	public Activator() {
		projectConfigs = new HashMap<IProject, TestProject>();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
	 * )
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		Logger.getLogger("").setLevel(Level.INFO);
		
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		workspace.addResourceChangeListener(new ProjectConfigFileModificationListener(), IResourceChangeEvent.POST_CHANGE);
		
		workspace.addSaveParticipant(PLUGIN_ID, new ClosingListener());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
	 * )
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
		FITTESTServerStartUp.getITEAgentInstance().stop();
		FITTESTServerStartUp.exitAgentGUIController();
		
		projectConfigs.clear();
		projectConfigs = null;
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Get the configuration of the currently selected FITTEST project. 
	 * 
	 * 
	 * @return a TestProject object if a fittest project is selected in the Package View
	 * @return null if no project is selected or the project configuration file has problem.
	 */
	public TestProject getActiveProjectConfig() {
		IProject selectedProject = getActiveProject();
		try {
			if (selectedProject!= null && selectedProject.getNature(TestingProject.NATURE_ID) != null) {
				TestProject projectConfig = projectConfigs.get(selectedProject);
				if (projectConfig == null){
					String configFile = selectedProject.getLocation().append("fittest.project").toOSString();
					projectConfig = ProjectConfigUtils.load(new File(configFile));
					projectConfigs.put(selectedProject, projectConfig);
				}
				return projectConfig;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Get the current active project selected in the package explorer
	 * @return
	 */
	public IProject getActiveProject(){
		if (PlatformUI.getWorkbench() != null && PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null){
			
			ISelectionService service = PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow().getSelectionService();
			ISelection s = service.getSelection(IFITTESTFolderConstants.NAVIGATOR);
			if (s != null) {
				IStructuredSelection structured = (IStructuredSelection) s;
				if (structured.getFirstElement() != null){
					IProject selectedProject = ((IResource) structured.getFirstElement()).getProject();
					
					recentSelectedProject = selectedProject;
					return selectedProject;
				}
			}
//		} else {
//			CommonNavigator navigator =(CommonNavigator) Viewer.getView(IFITTESTFolderConstants.NAVIGATOR);
//			
//			if(navigator!=null){
//				CommonViewer viewer = navigator.getCommonViewer();
//				if (viewer != null) {
//					ISelection selection = viewer.getSelection();
//					if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
//						IStructuredSelection sel = (IStructuredSelection) selection;
//						IProject selectedProject = ((IResource) sel.getFirstElement()).getProject();
//						recentSelectedProject = selectedProject;
//						return selectedProject;
//					}
//				}
//			}
		}
		
		// the last selected project
		if (recentSelectedProject != null)
			return recentSelectedProject;
		
		return null;
	}
	
	/**
	 * Get all the projects currently observed by the plugin 
	 * 
	 * @return
	 */
	public Set<IProject> getHandledProjects(){
		return projectConfigs.keySet();
	}

	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	public Image getImage(String key) {
		return getImageRegistry().get(key);
	}

	@Override
	protected void initializeImageRegistry(ImageRegistry registry) {
		registerImage(registry, IMG_HORIZONTAL, "th_horizontal.gif");
		registerImage(registry, IMG_VERTICAL, "th_vertical.gif");
		registerImage(registry, IMG_FORM_BG, "form_banner.gif");
	}

	private void registerImage(ImageRegistry registry, String key,
			String fileName) {
		try {
			IPath path = new Path("resources/icons/" + fileName);
			URL url = FileLocator.find(getBundle(), path, null);
			if (url != null) {
				ImageDescriptor desc = ImageDescriptor.createFromURL(url);
				registry.put(key, desc);
			}
		} catch (Exception e) {
		}
	}
	
	
	public IPath getPluginPath() {
		URL pluginPath = Platform.getBundle(PLUGIN_ID).getEntry("/");
		try {
			if (pluginPath != null) {
				pluginPath = FileLocator.resolve(pluginPath);
				return new Path(pluginPath.getPath());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		URL url = FileLocator.find(Platform.getBundle(PLUGIN_ID),
				new Path("/"), null);
		if (url != null) {
			return new Path(url.getPath());
		}

		return new Path("/");
	}

	/**
	 * Update the project configuration of the given project
	 * @param project
	 * @return
	 */
	public boolean updateProjectConfig(IProject project) {
		String configFile = project.getLocation().append("fittest.project").toOSString();
		TestProject projectConfig;
		try {
			projectConfig = ProjectConfigUtils.load(new File(configFile));
			projectConfigs.put(project, projectConfig);
			return true;
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return false;
	}

}
