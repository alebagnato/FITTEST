/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.tranformtools.wizards;

import java.io.ByteArrayInputStream;
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
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

import eu.fittest.tranformtools.Activator;

public class SeleniumGenWizard extends Wizard {
	SelenPage parameterPage = null;
	TestConfigPage testConfigPage = null;
	

	String cteFile;
	IProject currentProject;

	public SeleniumGenWizard(String cteFile, IProject project) {
		super();
		this.cteFile = cteFile;
		this.currentProject = project;
	}

	
	public boolean canFinish() {
		if (parameterPage == null || testConfigPage == null){
			return false;
		}
		
		if (getContainer().getCurrentPage().equals(testConfigPage)){
			return true;
		}
		
		if (parameterPage.isGenNewProject() 
				&& (Constants.FLEX_OBJECT_DRIVER.equals(parameterPage.getSeleniumDriver())
						|| Constants.FLASH_OBJECT_DRIVER.equals(parameterPage.getSeleniumDriver()))
				&& getContainer().getCurrentPage().equals(parameterPage) ){
			return false;
		}
		
		return true;
	}
	
	
	
	
	public boolean needsPreviousAndNextButtons() {
		if (parameterPage == null || testConfigPage == null){
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
		
		if (getContainer().getCurrentPage().equals(testConfigPage)){
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
					
					if (Constants.FLEX_OBJECT_DRIVER.equals(parameterPage.getSeleniumDriver())
							|| Constants.FLASH_OBJECT_DRIVER.equals(parameterPage.getSeleniumDriver())){
						// prepare a test config file
						prepareTestConfigFile(newProject);
					}
					
				} catch (CoreException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return true;
	}
	

	private void prepareTestConfigFile(IProject newProject) {
		
		IFile file = newProject.getFile("test.properties");
		if (!file.exists()){
			try {
				String templateGroupFile = Activator.getPluginPath().toOSString()  + "templates" + File.separator + "flex.testconfig.stg";
				
				STGroup stgGroup = new STGroupFile(templateGroupFile);
				ST tconfig = stgGroup.getInstanceOf("TestConfig");
				
				String targetPage = parameterPage.getTargetPage();
				// separate the URL Host and the Page
				int index = targetPage.lastIndexOf("/");
				if (index == -1) return;
				String url = targetPage.substring(0, index + 1);
				String page = targetPage.substring(index + 1, targetPage.length());
				
				tconfig.add("URL", url);
				tconfig.add("Page", page);
				
				//App,SeleniumHost="localhost",SeleniumPort="4444",SeleniumSpeed="100"
				tconfig.add("SeleniumHost", testConfigPage.seleniumServerHost);
				tconfig.add("SeleniumPort", testConfigPage.seleniumServerPort);
				tconfig.add("SeleniumSpeed", testConfigPage.seleniumSpeed);
				tconfig.add("SeleniumStartCmd", testConfigPage.seleniumBrowserStartCommand);
				tconfig.add("App", testConfigPage.applicationName);

				file.create(new ByteArrayInputStream(tconfig.render().getBytes()), false, null);
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (Exception e){
				e.printStackTrace();
			}
		}
	}

	
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		parameterPage = new SelenPage("paramsPage");
		parameterPage.setTitle("Generate Selenium test cases from CTE tree");
		parameterPage.setDescription("Specify required parameters");
		parameterPage.setImageDescriptor(imgDes);
		parameterPage.setCteFile(cteFile);
		
		addPage(parameterPage);
		
		testConfigPage = new TestConfigPage("configPage");
		testConfigPage.setTitle("Selenium and Flex/Flash test configuration");
		testConfigPage.setDescription("Specify configuration parameters (FLASH ONLY)");
		testConfigPage.setImageDescriptor(imgDes);
		
		addPage(testConfigPage);
	}
	

	public SelenPage getParameterPage() {
		return parameterPage;
	}

	public void setParameterPage(SelenPage parameterPage) {
		this.parameterPage = parameterPage;
	}

	public TestConfigPage getTestConfigPage() {
		return testConfigPage;
	}
	
	public void setTestConfigPage(TestConfigPage testConfigPage) {
		this.testConfigPage = testConfigPage;
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
		description.setNatureIds(new String[] { JavaCore.NATURE_ID });
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
		File seleniumJar = new File(depos.append("selenium-server-standalone.jar").toOSString());
		File startScript = new File(depos.append("start.sh").toOSString());
		File userExtensionFile = new File(depos.append("user-extensions.js").toOSString());
		
//		File targetLib = new File(libFolder.getLocation().toOSString());
		IPath targetFolder = libFolder.getLocation();
		try {
//			copyFile(junitJar, targetFolder.append("junit-4.8.2.jar").toFile());
			copyFile(seleniumJar, targetFolder.append("selenium-server-standalone.jar").toFile());
			
			if (parameterPage.getSeleniumDriver().equals(Constants.FLEX_OBJECT_DRIVER)
					|| parameterPage.getSeleniumDriver().equals(Constants.FLASH_OBJECT_DRIVER)){
				copyFile(fdriverAPIJar, targetFolder.append("fdrivers.jar").toFile());
				copyFile(startScript, targetFolder.append("start.sh").toFile());
				copyFile(userExtensionFile, targetFolder.append("user-extensions.js").toFile());
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
	
	
	
	/**
	 * 
	 * @param fin
	 * @param fout
	 * @throws Exception
	 */
	public void copyFolder(File fin, File fout) throws Exception {
		fout.mkdir();
		String[] children = fin.list();
		if (children == null) {
			// Either dir does not exist or is not a directory
		} else {
			for (int p = 0; p < children.length; p++) {
				File f = new File(fin + File.separator + children[p]);
				File f1 = new File(fout + File.separator + children[p]);
				if (f.isDirectory())
					copyFolder(f, f1);
				else
					copyFile(f, f1);
			}
		}
	}
	 
	/**
	 * 
	 * @param inputFile
	 * @param outputFile
	 */
	 public void copyFile(File inputFile, File outputFile) {
		int bufferSize = 4 * 1024;
		try {
			FileInputStream in = new FileInputStream(inputFile);
			FileOutputStream out = new FileOutputStream(outputFile);
			int readSize;
			byte buff[] = new byte[bufferSize];
			while ((readSize = in.read(buff)) != -1)
				out.write(buff, 0, readSize);
			in.close();
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
