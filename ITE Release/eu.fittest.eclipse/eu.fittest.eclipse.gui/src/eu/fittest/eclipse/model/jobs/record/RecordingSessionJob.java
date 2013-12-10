package eu.fittest.eclipse.model.jobs.record;

import java.util.List;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.environment.HostModel; // by urueda
import eu.fittest.eclipse.model.environment.IHostsListener; // by urueda
import eu.fittest.eclipse.model.jobs.ComponentDeployment;
import eu.fittest.eclipse.model.jobs.HUTManager;
import eu.fittest.eclipse.model.jobs.JobEvent;
import eu.fittest.eclipse.model.jobs.JobStatus;
import eu.fittest.eclipse.model.jobs.TestingSessionJob;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

//import eu.fittest.eclipse.component.junit.wizard.JUnitComponent; // by urueda

public class RecordingSessionJob extends TestingSessionJob implements IHostsListener{ 
	private class UpdateRecordingSession extends UpdateTask{
		@Override
		public void run() {
			long current = _elapsedTime+ System.currentTimeMillis() - _startedAt;
			_support.firePropertyChange("elapsedTime", 0, current);
			if(_totalDuration>0) setProgress(new Long(current*100L/_totalDuration).intValue());			
		}
	}
	
	private Long _totalDuration;
	private SessionTask _sessionTask;
	private final long _refreshRate;
	private HUTManager _hutManager;
	
	public Long getTotalDuration(){
		return _totalDuration;
	}
	
	private class SessionTask extends TimerTask{
		@Override
		public void run() {
			try {
				runToCompletion(JobEvent.Stop);
			} catch (FITTESTException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}		
	}
	
	// by urueda
	private class HostJoining implements Runnable { //extends Thread {
		private Host theHost;
		boolean paused, running;
		public HostJoining(Host h, boolean p, boolean r) {
			theHost = h;
			paused = p;
			running = r;
		}
		@Override
		public void run() {
			if (theHost.getEnvironment() == HUTEnvironment.TEST) { // wait for JUNIT component deployment
				synchronized(this) {
					try {
						boolean junitNotDeployed = true;
						IRegistrationService irs = FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(IRegistrationService.class);
						while (junitNotDeployed) {
							List<String> hostComponents = irs.getAllComponentsOnAgent(theHost.getName());
							wait(2000L); // wait for deployment
							for (String hostC : hostComponents) {
								if (hostC.startsWith("JUnitComponent")) { //JUnitComponentManager.JUNIT_COMPONENT.getName()
									junitNotDeployed = false;
								}
							}
						}
					} catch (InterruptedException ie) {
						Logger.getAnonymousLogger().log(Level.SEVERE, "Interrupted waiting for Host deployment: " + ie.toString());
					} catch (FITTESTException fe) {
						Logger.getAnonymousLogger().log(Level.SEVERE, "Fittest exception waiting for Host deployment: " + fe.toString());
					}
				}
			}
			if (paused) {
				try {
					_hutManager.initializeHUT(theHost);
				}
				catch (FITTESTException fe) {
					Logger.getAnonymousLogger().log(Level.WARNING,"Exception initializing host: " + theHost.toString());
				}
			}
			else if (running) {
				try {
					_hutManager.initializeHUT(theHost);
					_hutManager.startHUT(theHost);
				}
				catch (FITTESTException fe) {
					Logger.getAnonymousLogger().log(Level.WARNING, "Exception starting host: " + theHost.toString() + fe.toString());
				}
			}					
		}		
	}
	
	public RecordingSessionJob(IFolder session, Long duration, List<Host> HUTs, List<IFITTESTComponentManager> componentManagers) throws FITTESTException {
		this(session, duration, HUTs, new RecordingHUTManager(session, componentManagers, HUTs));
		HostModel.getInstance().addHostsListener(this); // by urueda
	}
	
	public RecordingSessionJob(IFolder session, Long duration, List<Host> HUTs, HUTManager manager) throws FITTESTException {
		super(session.getName());
		_totalDuration = duration;
		setProgress(-1);
		_refreshRate = 1000;//refresh every second
		_hutManager = manager;
	}
	
	public void setStatus(JobStatus status) throws FITTESTException{
		_support.firePropertyChange("status", _status , _status = status);
		switch (status) {
		case NEW:
			break;
		case RUNNING:
			runningState();
			break;
		case COMPLETED:
			stopState();
			break;
		case PAUSED:
			pauseState();
			break;
		case ABORTED:
			stopState();
			break;
		default:
			break;
		}
	}
	
	protected void runToCompletion(JobEvent event) throws FITTESTException{
		switch (event) {
		case Abort:		
			_hutManager.stopHUTs();
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			_hutManager.terminateHUTs();
			setStatus(JobStatus.ABORTED);
			break;
		case Pause:
			_hutManager.stopHUTs();
			setStatus(JobStatus.PAUSED);
			break;
		case Start:
			switch (_status) {
			case NEW:
				_hutManager.initializeHUTs();
				_hutManager.startHUTs();
				setStatus(JobStatus.RUNNING);
				break;
			case PAUSED:
				_hutManager.startHUTs();
				setStatus(JobStatus.RUNNING);
				break;
			default:
				break;
			}
			break;
		case Stop:
			_hutManager.stopHUTs();
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			_hutManager.terminateHUTs();
			setStatus(JobStatus.COMPLETED);
			HostModel.getInstance().removeHostsListener(this);
			break;
		default:
			break;
		}
	}
	
	private void stopState(){
		_timer.cancel();
		_elapsedTime += System.currentTimeMillis() - _startedAt;
		if(getProgress()<100) _updateTask.run();//fix interleaving issue
		_timer=null;
	}
	
	private void pauseState() throws FITTESTException{
		_sessionTask.cancel();
		_updateTask.cancel();
		_elapsedTime += System.currentTimeMillis() - _startedAt;
		if(_totalDuration>0 && _elapsedTime> _totalDuration) setStatus(JobStatus.COMPLETED);
	}
	
	protected void runningState(){
		setStartedAt(System.currentTimeMillis());
		_sessionTask = new SessionTask();
		_updateTask = new UpdateRecordingSession();
		_timer.scheduleAtFixedRate(_updateTask, _refreshRate, _refreshRate);
		if(_totalDuration>0) _timer.schedule(_sessionTask, _totalDuration - _elapsedTime);
	}

	@Override
	public void notifyHostAdded(Host host) {
		if (host.getType() == HUTType.CLIENT && _hutManager != null) {
			_hutManager.addHUT(host);
			final Host theHost = host;
			final boolean paused = _status == JobStatus.PAUSED;
			final boolean running = _status == JobStatus.RUNNING;
			//new HostJoining(theHost,paused,running).start();
			FITTESTSingleton.getThreadPool().execute(new HostJoining(theHost,paused,running));
		}
	}

	@Override
	public void notifyHostRemoved(Host host) {
		if (host.getType() == HUTType.CLIENT && _hutManager != null)
			_hutManager.removeHUT(host);
	}

	@Override
	public void notifyComponentAdded(Host host, String componentId) {
		if (_hutManager != null){
			_hutManager.addComponentOnHost(componentId, host);
		}
	}

	@Override
	public void notifyComponentRemoved(Host host, String componentId) {
		if (_hutManager != null){
			_hutManager.removeComponentOnHost(componentId, host);
		}
	}
	
	

}
