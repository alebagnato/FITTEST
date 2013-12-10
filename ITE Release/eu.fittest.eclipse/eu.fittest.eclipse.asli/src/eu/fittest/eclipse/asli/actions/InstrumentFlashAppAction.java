package eu.fittest.eclipse.asli.actions;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.osgi.framework.Bundle;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.eclipse.asli.Activator;
import eu.fittest.eclipse.asli.wizard.AsliWizard;

public class InstrumentFlashAppAction implements IObjectActionDelegate {
	private IFile _specification = null;

	private Shell _shell;
	
	private static Map<String, String> ASLI_EXE = new Hashtable<String, String>();
	
	static{
		ASLI_EXE.put("Mac OS_64bits", "asliMacOs");
		ASLI_EXE.put("Mac OS_32bits", "asliMacOs");
		ASLI_EXE.put("Windows_32bits", "asliWin32.exe");
		ASLI_EXE.put("Windows_64bits", "asliWin32.exe");//supposed that Windows 64 is still 32-bits compatible
		ASLI_EXE.put("Linux_64bits", "asliLinux64");
		ASLI_EXE.put("Linux_32bits", "asliLinux32");
		ASLI_EXE.put("x86", "32bits");
		ASLI_EXE.put("i386", "32bits");
		ASLI_EXE.put("x86_64", "64bits");
		ASLI_EXE.put("amd64", "64bits");
	}
	
	public InstrumentFlashAppAction() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void run(IAction action) {
		Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);
		try {
			final String platform = System.getProperty("os.name");
			final String arch = System.getProperty("os.arch");
			
			String exeName = null;
			if(platform.startsWith("Windows")){
				exeName = ASLI_EXE.get("Windows_" + ASLI_EXE.get(arch));
			}
			else if(platform.startsWith("Linux")){
				exeName = ASLI_EXE.get("Linux_" + ASLI_EXE.get(arch));
			}
			else if(platform.startsWith("Mac OS")){
				exeName = ASLI_EXE.get("Mac OS_" + ASLI_EXE.get(arch));
			}
			
			if(exeName == null){
				Display.getDefault().asyncExec(new Runnable() {
			        public void run() {
			            MessageDialog.openError(Display.getDefault().getActiveShell(),
			            		"Instrument Flash app", "No Asli binary is provided for your platform (" + platform + ", " + arch + ")\nAvailable ones:"+ASLI_EXE);
			        }
			    });
			}
			else{
				Logger.getAnonymousLogger().log(Level.INFO, "Selected Asli binary is "+ exeName);
				final File asli = new File(FileLocator.toFileURL(bundle.getEntry("resources/bin/"+exeName)).getFile());
				asli.setExecutable(true);
				if(asli.canExecute()){
					AsliWizard asliWizard = new AsliWizard(_specification);
					WizardDialog dialog = new WizardDialog(_shell, asliWizard);
					if (dialog.open() == WizardDialog.OK){
						try {
							Process process = Runtime.getRuntime().exec(asli.getAbsolutePath() +" "+_specification.getName() + " " +asliWizard.getParameterPage().getInputFile() +" " + asliWizard.getParameterPage().getOutputFile(), null, _specification.getParent().getLocation().toFile());
				            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getInputStream(), Level.INFO));
							FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getErrorStream(), Level.SEVERE));
				            process.waitFor();
							_specification.getProject().refreshLocal(IFolder.DEPTH_INFINITE, null);
						} catch (InterruptedException e) {
							Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());							
						} catch (CoreException e) {
							Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
						}
					}
				}
				else{
					Logger.getAnonymousLogger().log(Level.SEVERE, "Execution permission can't be set for file "+asli.getAbsolutePath());
					Display.getDefault().asyncExec(new Runnable() {
				        public void run() {
				            MessageDialog.openError(Display.getDefault().getActiveShell(),
				            		"Instrument Flash app", "Execution permission can't be set for file "+asli.getAbsolutePath());
				        }
				    });
				}
			}
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if(selection instanceof IStructuredSelection)
			_specification = (IFile) ((IStructuredSelection)selection).getFirstElement();	
	}

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		_shell = targetPart.getSite().getShell();
	}

}
