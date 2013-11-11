package eu.fittest.eclipse.searchinf.transformer.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import eu.fbk.xinputmining.XMLUtils;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.itelog.Body;
import eu.fittest.project.config.GAParameterType;
import eu.fittest.project.config.SUTTechnologyType;
import eu.fittest.project.config.TestProject;
import eu.fittest.ucl.api.ModelInferenceListener;
import eu.fittest.ucl.eventinf.api.EventBasedModelInference;
import eu.fittest.ucl.eventinf.api.ModelInferenceParameters;

public class OptSeqBasedInference  extends AbstractHandler implements IObjectActionDelegate{
	private IFolder logFolder = null;
	
	private static final int MAX_NUMBER_OF_FILES = 100;
	
	@Override
	public void run(IAction action) {
		final ModelInferenceParameters config = new ModelInferenceParameters();
		TestProject projectConfig = Activator.getDefault().getActiveProjectConfig();
		boolean isFlash = false;
		if (projectConfig!= null){
			GAParameterType gaParam = projectConfig.getModelInference().getGaParam();
			if (gaParam != null) {
				config.setMaxGenerations(gaParam.getMaxNumberOfGenerations());
				config.setPopulationSize(gaParam.getPopulationSize());
			}
			
			if (projectConfig.getGeneral().getType().equals(SUTTechnologyType.FLASH))
				isFlash = true;
		}
		final boolean includeTargetIDInEvents = isFlash;
		final File outputDir = new File(logFolder.getLocation().append("output").toOSString());
		try {
			if (!outputDir.exists())
				outputDir.mkdirs();
		} catch (Exception e){}
		
		
		Job generateJob = new Job("Search-enhanced Sequence-based Model inference"){
			
			@Override
			protected IStatus run(final IProgressMonitor monitor) {
				
				try {
					monitor.beginTask("Prepararing...", 1);
					List<IFile> allLogFiles = ResourceUtils.collectFiles(logFolder, "xml", "log_");
					if (allLogFiles.size() > 0){
						List<String> fileList = new ArrayList<String>();
						for (IFile f : allLogFiles){
							fileList.add(f.getLocation().toOSString());
						}
						
						List<Body> logBodies = XMLUtils.loadXMLLog(fileList, MAX_NUMBER_OF_FILES);
						monitor.done();
						EventBasedModelInference mi = new EventBasedModelInference();
						ModelInferenceListener listener = new ModelInferenceListener()
						{

							public void onStart() {
								monitor.subTask("Processing...");
							}

							public void progress(double percent, String message) {
								System.out.println("on progress:" + percent + "," + message);
								monitor.worked(1);
							}

							public void onStop() {
								monitor.done();
								System.out.println("on stooop");
								
							}
							
						};
						mi.registerListener(listener);
						monitor.beginTask("Infering model..", config.getMaxGenerations());
						mi.startInference(logBodies, outputDir, config, includeTargetIDInEvents);
						
					} else {
						
					}
				} catch (CoreException e) {
					inform("Error while loading log files!");
					e.printStackTrace();
				}
				
				try {
					logFolder.refreshLocal(IResource.DEPTH_INFINITE, null);
				} catch (CoreException e) {
					e.printStackTrace();
				}

				inform("Task done successfully!");
				return new Status(IStatus.OK, "State-based Model inference with auto-abstraction", IStatus.OK, "Done", null);
			}
		};
		generateJob.schedule();
	}
	
	private void inform(final String msg){
		Display.getDefault().asyncExec(new Runnable() {
	        public void run() {
	            MessageDialog.openInformation(Display.getDefault().getActiveShell(),
	            		"Search-Based Model Inference", msg);
	        }
	    });
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if(selection instanceof IStructuredSelection)
			logFolder = (IFolder) ((IStructuredSelection)selection).getFirstElement();	
		
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		logFolder = ResourceUtils.getFolder();
		run(null);
		return null;
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub
		
	}

}
