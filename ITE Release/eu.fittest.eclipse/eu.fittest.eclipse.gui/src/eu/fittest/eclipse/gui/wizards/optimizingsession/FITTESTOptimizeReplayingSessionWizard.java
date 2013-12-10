package eu.fittest.eclipse.gui.wizards.optimizingsession;

import java.beans.PropertyChangeListener;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.gui.utils.Viewer;
import eu.fittest.eclipse.gui.views.job.JobsView;
import eu.fittest.eclipse.gui.views.job.TableChangeListener;
import eu.fittest.eclipse.gui.wizards.AbstractSessionManagerWizard;
import eu.fittest.eclipse.gui.wizards.recordingsession.FITTESTRecordingSessionWizardPageTwo;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.JobModel;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.eclipse.model.jobs.optimize.OptimizingSessionJob;


public class FITTESTOptimizeReplayingSessionWizard extends AbstractSessionManagerWizard{
	
	public FITTESTOptimizeReplayingSessionWizard() {
		super("New Optimizing Sesssion");
	}
	
	@Override
	public void addPages() {
		this.pageOne = new FITTESTRecordingSessionWizardPageTwo(selectedProject,"New Optimizing Session");
		addPage(this.pageOne);
		
		addFITTESTComponentPages(SessionType.OptimizationSession);
	
	}

	protected void createJob(IFolder session, List<Host> HUTs) throws FITTESTException {
		OptimizingSessionJob job = new OptimizingSessionJob(session, ((FITTESTRecordingSessionWizardPageTwo)pageOne).getDuration(), HUTs, componentManagers);
		job.init();
		JobModel.INSTANCE.addJob(job);
		new OptimizingSessionManagerShell(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),job).open();
		PropertyChangeListener l = new TableChangeListener();
		job.addPropertyChangeListener("progress", l);
		job.addPropertyChangeListener("status", l);
		IViewPart view = Viewer.getView(JobsView.VIEW_ID);
		if(view instanceof JobsView){
			((JobsView)view).refresh();	
		}
	}

}
