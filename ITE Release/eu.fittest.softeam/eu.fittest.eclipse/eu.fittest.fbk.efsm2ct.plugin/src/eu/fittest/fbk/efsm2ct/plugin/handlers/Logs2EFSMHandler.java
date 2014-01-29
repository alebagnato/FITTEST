package eu.fittest.fbk.efsm2ct.plugin.handlers;

import java.io.File;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.handlers.HandlerUtil;
import org.osgi.service.log.LogService;

import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.tool.EFSM2DotStep;
import eu.fittest.fbk.efsm2ct.plugin.tool.Logs2EFSMStep;
import eu.fittest.fbk.efsm2ct.plugin.tool.ToolStepException;
import eu.fittest.fbk.efsm2ct.plugin.utils.GuiHelper;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;
import eu.fittest.fbk.efsm2ct.plugin.wizards.log2efsm.Logs2EFSMGenerationWizard;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class Logs2EFSMHandler extends Efsm2CtAbstractHandler {
	
	/**
	 * The constructor.
	 */
	public Logs2EFSMHandler() {
	}

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
		
		
		ISelection mySelection = HandlerUtil.getCurrentSelection(event);
		
		// logger.info("run:" + event);

		if (!mySelection.isEmpty()) {

			StructuredSelection selection = (StructuredSelection) mySelection;
			IFolder inputFolder = (IFolder) selection.getFirstElement();
			
			List<IResource> found;
			try {
				found = ResourceUtils.match(inputFolder, "efsm.properties");
			} catch (CoreException e1) {
				Activator.getDefault().osgiLog(LogService.LOG_ERROR, "error in EFSM2MonitorAction: no conf file found in"+inputFolder);
				throw new ExecutionException("No configuration file found");
			}
			
			if (found.size() != 1) {
				
				Activator.getDefault().osgiLog(LogService.LOG_ERROR, "error in EFSM2MonitorAction: no conf file found in"+inputFolder);
				throw new ExecutionException("No configuration file found");
				
			}
			
			IFile propertiesFile = (IFile) found.get(0);

			IProject project = inputFolder.getProject();

			// IFolder modelsFolder = project.getFolder(IFITTESTFolderConstants.MODELS);

			Logs2EFSMGenerationWizard wiz = new Logs2EFSMGenerationWizard(inputFolder, inputFolder);
			
			WizardDialog wizDialog = new WizardDialog(HandlerUtil.getActiveShell(event), wiz);

			if (wizDialog.open() == WizardDialog.OK) {

				try {

					// String modelFilePath = wiz.getOutput(); 
					// the above is expected to have a structure like
					// <none or some path> <name> .efsm
					
					String userFileString = wiz.getUserFileString();
					File inputFolderFile = inputFolder.getLocation().toFile();
					
					File filterFile = propertiesFile.getLocation().toFile();
					
					log2EfsmStep(inputFolderFile, userFileString, filterFile);

				} catch (Exception e) {
					Activator.getDefault().osgiLog(LogService.LOG_ERROR, "error in EFSM2MonitorAction", e);
					GuiHelper.showError(HandlerUtil.getActiveShell(event), "Error generating EFSM monitor", e);
				}

				refresh(project);

			}

		}
		
		return null;

	}
	


	public static void log2EfsmStep(File inputFolderFile,
			String userFileString, File filterFile) throws ToolStepException {
		
		Logs2EFSMStep step1 = new Logs2EFSMStep();
		
		step1.setInputFolderFile(inputFolderFile);
		
		File userFsmFile = new File(userFileString); // model file path as typed in by the user
		
		File outputDir = new File(inputFolderFile, "output");
		
		outputDir.mkdirs();
		
		File outputFsmFile = new File(outputDir,userFileString);
		
		String efsmName = userFsmFile.getName().replace(".efsm", ""); 
		
		step1.setOutputFsmFile(outputFsmFile);
		
		String efsmPackage = inputFolderFile.getName();
		
		step1.setFsmPackage(ResourceUtils.clearForPackageName(efsmPackage));
		step1.setFsmName(efsmName);
		step1.setFilterFile(filterFile);
		
		step1.execute();
		
		EFSM2DotStep step2 = new EFSM2DotStep();
		step2.setFsmFilePath(outputFsmFile.getPath());
		step2.setDestDirName("output");
		
		step2.execute();
	}

	
}
