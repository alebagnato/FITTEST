package eu.fittest.tranformtools.actions;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PlatformUI;

import eu.fittest.modelInference.fsmInference.config.Config_Launcher;
import eu.fittest.modelInference.fsmInference.manager.FSMmanager;
import eu.fittest.modelInference.logConverter.Converter;
import eu.fittest.modelInference.logConverter.Utility;
import eu.fittest.tranformtools.Activator;
import eu.fittest.tranformtools.wizards.FSMGenWizard;

public class Log2FSMAction implements IWorkbenchWindowActionDelegate {

	IWorkbenchWindow activeWindow;
	
	private String inputFolder;
	private String outputFSM;
	private boolean genDot = true;
	
	
	
	public void process(){
		final String inputFolder = this.inputFolder;
		final String outputFSM = this.outputFSM;
		final boolean genDotFile = this.genDot;
		
		final FSMmanager fsmmanager=new FSMmanager();
		final Config_Launcher cl= Config_Launcher.getInstace();
		
		activeWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		
		Job infererringJob = new Job("Inferring FSM model from logs"){
			
			protected IStatus run(IProgressMonitor monitor) {
				String activeInputFolder = inputFolder;
				if (isXMLFormat(inputFolder)){
					activeInputFolder = convert2FBKFormat(inputFolder, monitor);
					if (activeInputFolder == null){
						 return new Status(IStatus.ERROR, "FITTEST tranformation plugin", 
									IStatus.ERROR, "Problem with log conversion!", null);
					}
				}
				
				File[] fileList=fsmmanager.getFilelist(activeInputFolder, cl.maxFilePemutations);
//				File[] fileCurrent=new File[1];
//				
//				monitor.beginTask("Inferring FSM model", fileList.length);
//				
//				for (int logIndex = 0; logIndex < fileList.length; logIndex++) {
//					fileCurrent[0]=fileList[logIndex];
//					
//					monitor.subTask(fileCurrent[0].getName());
//					monitor.worked(1);
//					
//					if (logIndex==0) fsmmanager.generateFSM(fileCurrent, true, 0);
//					else fsmmanager.generateFSM(fileCurrent, false, 0);
//				}
				fsmmanager.generateFSM(fileList, false, 0);
				monitor.done();
				monitor.beginTask("Saving FSM model", 1);
				fsmmanager.FSM2FsmXpr(outputFSM, null, null);
				
				if (genDotFile){
					String dotFileName = outputFSM.replace(".fsm", ".dot");
					fsmmanager.FSM2DOT(dotFileName);
				}
				monitor.done();
				
				activeWindow.getShell().getDisplay().asyncExec(new Runnable() {
					public void run() {
						MessageDialog.openInformation(activeWindow.getShell(), "Model inference", 
						"Model inference completed, check the specified output file for result !");
					}
				});
				
				return new Status(IStatus.OK, "FITTEST tranformation plugin", 
							IStatus.OK, "Finish inferring model from logs!", null);
			}
			
		};
		
		infererringJob.schedule();
	}
	
	public void run(IAction action) {
		FSMGenWizard wizard = new FSMGenWizard();
		WizardDialog dialog = new WizardDialog(activeWindow.getShell(), wizard);
		
		if (dialog.open() == WizardDialog.OK){
			this.inputFolder = wizard.getParamPage().getInputFolder();
			this.outputFSM = wizard.getParamPage().getOutputModelFile();
			this.genDot = wizard.getParamPage().isGenDotFile();
			process();
		}
	}
	
	/**
	 * Check if log files are in XML
	 * @param inputFolder
	 * @return
	 */
	private boolean isXMLFormat(String inputFolder){
		Utility utils=new Utility();
		File[] fileList=utils.getFileList(inputFolder);
		List<File> fileToBeChecked = new ArrayList<File>();
		
		if (fileList.length == 0) return false;
		
		if (fileList.length > 3){
			Random ranSelector = new Random();
			File f = fileList[ranSelector.nextInt(fileList.length)];
			if (f != null)
				fileToBeChecked.add(f);
			f = fileList[ranSelector.nextInt(fileList.length)];
			if (f != null)
				fileToBeChecked.add(f);
		} else {
			for (File f : fileList){
				if (f!=null)
					fileToBeChecked.add(f);
			}
		}
		
		for (File f : fileToBeChecked){
			if (f.isFile()){
				try {
					BufferedReader reader = new BufferedReader(new FileReader(f));
					String firstLine = reader.readLine();
					reader.close();
					if (!firstLine.startsWith("<?xml")){
						return false;
					}
					
				} catch (FileNotFoundException e) {
					return false;
				} catch (IOException e) {
					return false;
				}
			}
		}
		
		return true;
	}
	
	/**
	 * Convert XML format log to FBK legacy format
	 * Temporary folder return should be removed or empty
	 * @param inputFolder
	 * @return tempFolder
	 */
	private String convert2FBKFormat(String inputFolder, IProgressMonitor monitor){
		
		final IPath tmpFolder = Activator.getDefault().getStateLocation().append("tmp"); //
//		final IPath tmpFolder = Activator.getPluginPath().append("tmplogs");
		
		File f = new File(tmpFolder.toOSString());
		try {
			if (f.exists()){
				deleteDir(f); // empty directory
			}
			f.mkdirs();
		} catch (Exception e) {
			activeWindow.getShell().getDisplay().syncExec(new Runnable() {
				
				public void run() {
					MessageDialog.openError(activeWindow.getShell(),
									"Log coverting",
									"Cannot write to tmp folder during log conversion\n" + tmpFolder.toOSString());
						
				}
			});
			return null;
		}
		
		Converter uu2fbkLogs=new Converter();
		Utility utils=new Utility();
		File[] fileList=utils.getFileList(inputFolder);
//		monitor.beginTask("Converting log", fileList.length);
		for (int i = 0; i < fileList.length; i++) {
			if (fileList[i] != null) {
//				String outputFileName = tmpFolder.append(fileList[i].getName()).addFileExtension("txt").toOSString();
				String outputFileName = tmpFolder.append(fileList[i].getName()).toOSString();
				if (fileList[i].isFile()) uu2fbkLogs.convert(fileList[i],outputFileName);
//				monitor.worked(1);
			}
		}
//		monitor.done();
		
		return tmpFolder.toOSString();
	}

	/**
	 * Empty a directory
	 * @param dir
	 * @return
	 */
	private boolean deleteDir(File dir) {
	    if (dir.isDirectory()) {
	        String[] children = dir.list();
	        for (int i=0; i<children.length; i++) {
	            boolean success = deleteDir(new File(dir, children[i]));
	            if (!success) {
	                return false;
	            }
	        }
	    }

	    // The directory is now empty so delete it
	    return dir.delete();
	}
	
	
	
	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub
		
	}

	
	public void dispose() {
		// TODO Auto-generated method stub
		
	}

	
	public void init(IWorkbenchWindow window) {
		this.activeWindow = window;
	}

	public Log2FSMAction(String inputFolder, String outputFSM, boolean genDot) {
		super();
		this.inputFolder = inputFolder;
		this.outputFSM = outputFSM;
		this.genDot = genDot;
	}

	
	

}
