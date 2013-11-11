package eu.fittest.eclipse.model.jobs.optimize;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;

import eu.fittest.agent.ite.services.progress.spec.IProgressService;
import eu.fittest.agent.ite.services.progress.spec.IProgressServiceListener;
import eu.fittest.agent.ite.services.progress.spec.ProgressEvent;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.JobEvent;
import eu.fittest.eclipse.model.jobs.JobStatus;
import eu.fittest.eclipse.model.jobs.record.RecordingSessionJob;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class OptimizingSessionJob extends RecordingSessionJob{	
	private ProgressListener _l;
	
	public OptimizingSessionJob(IFolder session, Long duration, List<Host> HUTs, List<IFITTESTComponentManager> componentManagers) throws FITTESTException {
		super(session,duration, HUTs,new OptimizingHUTManager(session, componentManagers, HUTs));
	}
	
	public void setStatus(JobStatus status) throws FITTESTException {
		super.setStatus(status);
		if(getTotalDuration()==0L){
			switch (status) {
			case NEW:
				_l = new ProgressListener();
				FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(IProgressService.class).addServiceListener(_l);
				break;
			case RUNNING:
			
				break;
			case ABORTED:
				FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(IProgressService.class).removeServiceListener(_l);
				break;
			case COMPLETED:
				FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(IProgressService.class).removeServiceListener(_l);
				break;
			case PAUSED:
				break;
			default:
				break;
			}
		}
	}
	
	private class ProgressListener implements IProgressServiceListener{

		@Override
		public synchronized void incomingEvent(ProgressEvent event) {
			setProgress(event.getProgress());
			if(event.getProgress()>=100)
				try {
					runToCompletion(JobEvent.Stop);
				} catch (FITTESTException e) {
					Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
				}
		}
		
	}
}
