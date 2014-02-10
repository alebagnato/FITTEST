package eu.fittest.fbk.efsm2ct.plugin.handlers;

//import java.util.logging.Level;
//import java.util.logging.Logger;

import java.io.File;
import java.io.FileInputStream;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;
import org.osgi.service.log.LogService;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;
import eu.fittest.fbk.efsm2ct.plugin.tool.EFSM2MonitorStep;
import eu.fittest.fbk.efsm2ct.plugin.utils.GuiHelper;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;


public class EFSM2MonitorHandler extends Efsm2CtAbstractHandler {

	private static final String GENSRC = "gensrc";
	private static final String OUTPUT_ROOT_DIR = "output";

	// private static Logger logger = Logger.getAnonymousLogger();
	
	 

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

        ISelection mySelection = HandlerUtil.getCurrentSelection(event);
        
		
		// logger.info("run:" + event);

		if (!mySelection.isEmpty()) {

			StructuredSelection selection = (StructuredSelection) mySelection;

			IFolder fe = (IFolder) selection.getFirstElement();
			IProject project = fe.getProject();
			
			IFolder outputFolder = (IFolder) fe.findMember(OUTPUT_ROOT_DIR);
			
			List<IResource> matched;
			
			try {
				matched = ResourceUtils.match(outputFolder,"^.*\\.efsm$");
			} catch (CoreException e1) {
				throw new ExecutionException("internal error",e1);
			}
			
			if (matched.size() != 1) {
				throw new ExecutionException("internal error: invalid number of resource selected:"+matched.size());
			}

			String fsmFilePath = matched.get(0).getLocation().toFile().getAbsolutePath();

			String packagePrefix = "";
			
			ensureDirectoryExists(GENSRC, outputFolder);
			File destDir = outputFolder.getFolder(GENSRC).getLocation().toFile();
			
			String destDirName = destDir.toString();
			
			try {
				
				Properties props = getEfsmProperties(fe);
							
				String generateDriverStr = props.getProperty("efsm2ct.generate.driver");
				boolean generateDriver = Boolean.parseBoolean(generateDriverStr);

				EFSM2MonitorStep myStep = new EFSM2MonitorStep();

				myStep.setDestDirName(destDirName);
				myStep.setFsmFilePath(fsmFilePath);
				myStep.setPackagePrefix(packagePrefix);
				myStep.setGenerateActualDriver(generateDriver);

				myStep.execute();
				
				String prefix = fe.getLocation().toString().replace(project.getLocation().toString(), "");
				
				
				addDirectoryToSourcePath(project, prefix+File.separator+OUTPUT_ROOT_DIR+File.separator+GENSRC);

			} catch (Exception e) {
				Activator.getDefault().osgiLog(LogService.LOG_ERROR, "Error generating EFSM monitor", e);
				GuiHelper.showError(HandlerUtil.getActiveShell(event), "Error generating EFSM monitor",
						e);
			}

			refresh(project);

		}
		
		

		return null;
	}

	private void addDirectoryToSourcePath(IProject project, String destDir) throws JavaModelException {
		
		IJavaProject javaProject = JavaCore.create(project);
		IClasspathEntry[] entries = javaProject.getRawClasspath();
		
		boolean found = false;
		
		for (IClasspathEntry e : entries) {
			
			// System.out.println("compare:"+e.getPath()+" "+destDir);
			
			if (e.getPath().toString().endsWith(destDir)) {
				found = true;
				break;
			}
			
		}

		if (!found) {
		
		IClasspathEntry[] newEntries = new IClasspathEntry[entries.length + 1];
		System.arraycopy(entries, 0, newEntries, 0, entries.length);

		IPath srcPath= javaProject.getPath().append(destDir);
		IClasspathEntry srcEntry= JavaCore.newSourceEntry(srcPath, null);

		newEntries[entries.length] = JavaCore.newSourceEntry(srcEntry.getPath());
		javaProject.setRawClasspath(newEntries, null);
		
		}
		
	}

}
