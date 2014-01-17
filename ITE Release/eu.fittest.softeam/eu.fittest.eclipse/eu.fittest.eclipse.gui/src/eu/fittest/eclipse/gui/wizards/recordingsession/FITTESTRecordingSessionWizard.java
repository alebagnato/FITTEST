package eu.fittest.eclipse.gui.wizards.recordingsession;

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
import eu.fittest.eclipse.gui.wizards.FITTESTWizardMessages;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.JobModel;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.eclipse.model.jobs.record.RecordingSessionJob;

public class FITTESTRecordingSessionWizard extends AbstractSessionManagerWizard{
	
	private static final String WIZARD_NAME= FITTESTWizardMessages.FITTESTRecordingSessionWizard_Name;
	
	public FITTESTRecordingSessionWizard() {
		super(WIZARD_NAME);
	}
	
	@Override
	public void addPages() {
		pageOne = new FITTESTRecordingSessionWizardPageTwo(selectedProject);
		addPage(pageOne);
		
		addFITTESTComponentPages(SessionType.RecordingSession);
	}

	protected void createJob(IFolder session, List<Host> HUTs) throws FITTESTException {
		RecordingSessionJob job = new RecordingSessionJob(session, ((FITTESTRecordingSessionWizardPageTwo)pageOne).getDuration(), HUTs, componentManagers);
		job.init();
		JobModel.INSTANCE.addJob(job);
		new RecordingSessionManagerShell(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),job).open();
		PropertyChangeListener l = new TableChangeListener();
		job.addPropertyChangeListener("progress", l);
		job.addPropertyChangeListener("status", l);
		IViewPart view = Viewer.getView(JobsView.VIEW_ID);
		if(view instanceof JobsView){
			((JobsView)view).refresh();	
		}
	}

}
