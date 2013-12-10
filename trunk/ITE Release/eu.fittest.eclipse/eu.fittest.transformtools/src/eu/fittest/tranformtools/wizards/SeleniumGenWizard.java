package eu.fittest.tranformtools.wizards;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.nature.TestingProject;
import eu.fittest.tranformtools.Activator;
import eu.fittest.tranformtools.utils.LogFileUtils;

public class SeleniumGenWizard extends Wizard {
	SelenPage parameterPage = null;
	

	IFile cteFile;
	IProject currentProject;

	public SeleniumGenWizard(IFile cteFile, IProject project) {
		super();
		this.cteFile = cteFile;
		this.currentProject = project;
	}
	
	
	public boolean needsPreviousAndNextButtons() {
		if (parameterPage == null){
			return true;
		}
		
		if (getContainer().getCurrentPage() == null){
			return true;
		}
		
		if (parameterPage.isGenNewProject() 
				&& (Constants.FLEX_OBJECT_DRIVER.equals(parameterPage.getSeleniumDriver())
						|| Constants.FLASH_OBJECT_DRIVER.equals(parameterPage.getSeleniumDriver()))
				&& getContainer().getCurrentPage().equals(parameterPage) ){
			return true;
		}
		
		return false;
	}

	
	public boolean performFinish() {
		if (parameterPage == null)
			return false;
		else {
			if (parameterPage.isGenNewProject()){
				try {
					IProject newProject = createNewProject();
					String outputFolder = newProject.getFolder("src").getLocation().toOSString();
					parameterPage.setOutputFolder(outputFolder);
					
				} catch (CoreException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return true;
	}

	
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		parameterPage = new SelenPage("paramsPage");
		parameterPage.setTitle("Generate Selenium test cases from CTE tree");
		parameterPage.setDescription("Specify required parameters");
		parameterPage.setImageDescriptor(imgDes);
		parameterPage.setCteFile(cteFile.getLocation().toOSString());
		
		File parentFolder = cteFile.getParent().getLocation().toFile();
		if(!parentFolder.getName().equals(IFITTESTFolderConstants.MODELS)){
			parameterPage.setPackageName(parentFolder.getName());
		}
		
		String className = cteFile.getName().substring(0, cteFile.getName().lastIndexOf(".cte"));
		switch (className.length()) {
		case 0:
			break;
		case 1:
			className = className.substring(0,1).toUpperCase();
			break;
		default:
			className = className.substring(0,1).toUpperCase()+className.substring(1);
			break;
		}
		parameterPage.setClassName(className);
		parameterPage.setOutputFolder(currentProject.getFile(IFITTESTFolderConstants.TEST_SUITES).getLocation().toOSString());
		addPage(parameterPage);
		
	}
	

	public SelenPage getParameterPage() {
		return parameterPage;
	}

	public void setParameterPage(SelenPage parameterPage) {
		this.parameterPage = parameterPage;
	}

	/**
	 * 
	 * @return
	 * @throws CoreException
	 */
	private IProject createNewProject() throws CoreException{
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		// get current project name
		
		String generatedProject =  currentProject.getName() + ".fittest.selenium";
		
		IProject project = root.getProject(generatedProject);
		project.create(null);
		project.open(null);
		
		IProjectDescription description = project.getDescription();
		description.setNatureIds(new String[] { JavaCore.NATURE_ID, TestingProject.NATURE_ID });
		project.setDescription(description, null);
		
		IJavaProject javaProject = JavaCore.create(project); 
		
//		IFolder binFolder = project.getFolder("bin");
//		binFolder.create(true, true, null);
//		javaProject.setOutputLocation(binFolder.getLocation(), null);
		
		List<IClasspathEntry> entries = new ArrayList<IClasspathEntry>();
		
		// system libs
		
//		IVMInstall vmInstall = JavaRuntime.getDefaultVMInstall();
//		LibraryLocation[] locations = JavaRuntime.getLibraryLocations(vmInstall);
//		for (LibraryLocation element : locations) {
//			entries.add(JavaCore.newLibraryEntry(element.getSystemLibraryPath(), null, null));
//		}
		entries.add(JavaRuntime.getDefaultJREContainerEntry());
		
		// add junit 4
		IPath path = new Path("org.eclipse.jdt.junit.JUNIT_CONTAINER/4"); 
		entries.add(JavaCore.newContainerEntry(path));
		
		IFolder libFolder = project.getFolder("lib");
		libFolder.create(false, true, null);

		// copy selenium and junit jar to lib
		
		IPath depos = Activator.getPluginPath().append("depos");
		File fdriverAPIJar = new File(depos.append("fdrivers.jar").toOSString());
//		File junitJar = new File(depos.append("junit-4.8.2.jar").toOSString());
		File seleniumJar = new File(depos.append("selenium-server-standalone-2.15.0.jar").toOSString());
		File startScript = new File(depos.append("start.sh").toOSString());
		File userExtensionFile = new File(depos.append("user-extensions.js").toOSString());
		
//		File targetLib = new File(libFolder.getLocation().toOSString());
		IPath targetFolder = libFolder.getLocation();
		try {
//			copyFile(junitJar, targetFolder.append("junit-4.8.2.jar").toFile());
			LogFileUtils.copyFile(seleniumJar, targetFolder.append("selenium-server-standalone-2.15.0.jar").toFile());
			
			if (parameterPage.getSeleniumDriver().equals(Constants.FLEX_OBJECT_DRIVER)
					|| parameterPage.getSeleniumDriver().equals(Constants.FLASH_OBJECT_DRIVER)){
				LogFileUtils.copyFile(fdriverAPIJar, targetFolder.append("fdrivers.jar").toFile());
				LogFileUtils.copyFile(startScript, targetFolder.append("start.sh").toFile());
				LogFileUtils.copyFile(userExtensionFile, targetFolder.append("user-extensions.js").toFile());
			}
			
			String[] jars =  (new File(targetFolder.toOSString())).list(new FilenameFilter() {
				
				
				public boolean accept(File dir, String name) {
					if (name.endsWith("jar"))
						return true;
					return false;
				}
			});
			
			for (String jar : jars){
				path = libFolder.getLocation().append(jar);
//				entries.add(JavaCore.newLibraryEntry(new Path("lib" + File.separatorChar + jar), null, null, true));
				entries.add(JavaCore.newLibraryEntry(path, null, null));
				
			}
		
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		//add  libs to project class path
		javaProject.setRawClasspath(entries.toArray(new IClasspathEntry[entries.size()]), null);
		
		IFolder sourceFolder = project.getFolder("src");
		sourceFolder.create(false, true, null);
		
		IPackageFragmentRoot sourceRoot = javaProject.getPackageFragmentRoot(sourceFolder);
		IClasspathEntry[] oldEntries = javaProject.getRawClasspath();
		IClasspathEntry[] newEntries = new IClasspathEntry[oldEntries.length + 1];
		System.arraycopy(oldEntries, 0, newEntries, 0, oldEntries.length);
		newEntries[oldEntries.length] = JavaCore.newSourceEntry(sourceRoot.getPath());
		javaProject.setRawClasspath(newEntries, null);

		return project; // sourceFolder.getLocation().toOSString();
	}
	

}
