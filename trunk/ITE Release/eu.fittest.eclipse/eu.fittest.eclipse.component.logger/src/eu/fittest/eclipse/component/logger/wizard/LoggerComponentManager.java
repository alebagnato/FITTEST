package eu.fittest.eclipse.component.logger.wizard;

import java.util.HashMap;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.RequestProperties;
import eu.fittest.common.core.xml.RequestProperties.Property;
import eu.fittest.common.core.xml.UserDialog;
import eu.fittest.component.appdescription.ite.services.properties.spec.AvailableProperties;
import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesService;
import eu.fittest.component.appdescription.ite.services.properties.spec.PropertiesEvent;
import eu.fittest.eclipse.component.AbstractFITTESTComponentManager;
import eu.fittest.eclipse.component.ComponentInformation;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.converter.Log2XMLBatchConverter;
import eu.fittest.eclipse.component.logger.Activator;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.environment.HostStates;
import eu.fittest.eclipse.model.jobs.ComponentDeployment;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.project.config.SUTTechnologyType;
import eu.fittest.project.config.TestProject;

public class LoggerComponentManager extends AbstractFITTESTComponentManager{
	private LoggerPage _page = null;
	private HashMap<String, String> _sync; 
	private HashMap<String,Boolean> _nbHost; 
	private HashMap<Host,Integer> _uploadPeriods; 

	public static final ComponentInformation LOGGER_COMPONENT = new ComponentInformation("eu.fittest.Logging","", Activator.PLUGIN_ID);
	
	public LoggerComponentManager() {
		super(LOGGER_COMPONENT);
		_sync = new HashMap<String,String>(); 
	}
	
	@Override
	public IFITTESTComponentWizardPage[] getConfigurationPages() {
		TestProject activeProjectConfig = eu.fittest.eclipse.gui.Activator.getDefault().getActiveProjectConfig();
		if (activeProjectConfig != null)
			if(_page==null) _page = new LoggerPage(true, activeProjectConfig.getLogging().getLogTarget().getLogLevel());
		return new IFITTESTComponentWizardPage[]{_page};
	}
	
	private class InitializationTask implements Runnable {
		private Host _host;
		
		public InitializationTask(Host host) {
			_host = host;
		}
		
		@Override
		public void run() {
			try {					
				UserDialog message = FITTESTSingleton.getObjectFactory().createUserDialog();
				message.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
				message.setTo(_host.getName());
				String value = waitForValue(AvailableProperties.SELENIUM);
				if(value!=null){
					message.setMessage("A recording session is about to start.\nPlease open your browser at "+value);
					message.setType(JOptionPane.INFORMATION_MESSAGE);
					_registry.findService(IConnectionService.class).sendMessage(message);
					
					waitForDeployment(_host,LOGGER_COMPONENT.getName());
					
					Initialize initialize = FITTESTSingleton.getObjectFactory().createInitialize();
					initialize.getParameter().addAll(_page.getInitializationParameters());
					sendMessage(initialize, _host,LOGGER_COMPONENT.getName());//message won't be sent if component does not exist
					
				}
			}  catch (FITTESTException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
			synchronized(_sync.get(_host.getName())) { 
				_nbHost.put(_host.getName(),new Boolean(false));
				_sync.get(_host.getName()).notifyAll(); 
			}
		}	
	}
	
	private class StartThread implements Runnable {
		private Host host;
		public StartThread(Host h) {
			host = h;
		}
		@Override
		public void run() {
			switch (_type) {
			case ReplayingSession:
				break;
			case RecordingSession:
				if(deploymentCondition(host, LOGGER_COMPONENT.getName())){
					String value = waitForValue(AvailableProperties.SELENIUM);
					if(value!=null){
						synchronized(_sync.get(host.getName())) { 
							Boolean b = _nbHost.get(host.getName()); 
							if (b != null && b) { 
								try {
									_sync.get(host.getName()).wait(); 
								} catch (InterruptedException e) {
									Logger.getAnonymousLogger().log(Level.SEVERE, "Interrupted waiting for host initialization: " + host.getName()); 
								}
							}
							String shn = _sync.get(host.getName());  
							_nbHost.remove(host.getName()); 
							_sync.remove(host.getName()); 
							shn.notifyAll(); 
						}
						try {
							if(ComponentDeployment.getInstance().findComponent(host.getName(), LOGGER_COMPONENT.getName())!=null){
								sendMessage(FITTESTSingleton.getObjectFactory().createStart(), host, LOGGER_COMPONENT.getName());
							}
							else{
								//throw new FITTESTException("Time-out: No FITTEST component connects to the ITE");
								Logger.getAnonymousLogger().log(Level.WARNING, "Time-out: No FITTEST component connects to the ITE"); 
							}
						} catch (FITTESTException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					else{
						//throw new FITTESTException("App description URL has not been set on the server");
						Logger.getAnonymousLogger().log(Level.SEVERE, "App description URL has not been set on the server"); 
					}
				}
				break;
			default:
				break;
			}
		}
	}
	
	@Override
	public void initialize(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			synchronized(_sync) {
				if (_sync.containsKey(host.getName())) { // there is an initialization/start of the same host ongoing (by urueda)
					try {
						_sync.get(host.getName()).wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
//						ITELogger.log(Level.SEVERE, "Interrupted waiting for ongoing initialization of host: " + host.getName());
					}
				}
				_sync.put(host.getName(), host.getName());
			}
			if(deploymentCondition(host,LOGGER_COMPONENT.getName())){
				synchronized(_sync.get(host.getName())) { 
					_nbHost.put(host.getName(),new Boolean(true)); 
				}
				FITTESTSingleton.getThreadPool().execute(new InitializationTask(host));//threaded so that the SELENIUM property can be requested
			}
			if((host.getType().equals(HUTType.SERVER)||host.getType().equals(HUTType.MIXED)) &&
					ComponentDeployment.getInstance().findComponent(host.getName(), "AppDescriptionComponent")!=null){
				requestProperty(host, AvailableProperties.SELENIUM);
			}
			break;
		default:
			break;
		}		
	}
	
	protected void requestProperty(Host h, String... propertyNames) throws FITTESTException{
		_registry.findService(IPropertiesService.class).addServiceListener(this);
		RequestProperties message = FITTESTSingleton.getObjectFactory().createRequestProperties();
		for(String name: propertyNames){
			Property p = FITTESTSingleton.getObjectFactory().createRequestPropertiesProperty();
			p.setName(name);
			message.getProperty().add(p);
		}
		message.setTo(ComponentDeployment.getInstance().findComponent(h.getName(), "AppDescriptionComponent"));
		message.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
		_registry.findService(IConnectionService.class).sendMessage(message);
	}
	
	synchronized protected String waitForValue(String property){
		IPropertiesService service = _registry.findService(IPropertiesService.class);
		if(!service.isPropertySet(property)){
			try {
				wait(10000L);
			} catch (InterruptedException e) {
			}
		}
		_registry.findService(IPropertiesService.class).removeServiceListener(this);
		return service.getProperty(property);
	}

	@Override
	public void deployOn(Host host) throws FITTESTException {
		//component is not deployed by plugin
	}
	
	@Override
	protected boolean deploymentCondition(Host host, String componentName) {
		// should check if the session is recording
//		return _page.isEnabled() && (host.getType().equals(HUTType.CLIENT)  || host.getType().equals(HUTType.MIXED));
		return getSUTTechOfSelectedProject().equals(SUTTechnologyType.FLASH) && (host.getType().equals(HUTType.CLIENT)  || host.getType().equals(HUTType.MIXED));
	}

	@Override
	public void start(Host host) throws FITTESTException {
		FITTESTSingleton.getThreadPool().execute(new StartThread(host)); 
	}
	
	@Override
	public void stop(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			sendMessage(FITTESTSingleton.getObjectFactory().createStop(), host, LOGGER_COMPONENT.getName());
			break;
		default:
			break;
		}			
	}
	
	@Override
	public void terminate(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			sendMessage(FITTESTSingleton.getObjectFactory().createTerminate(), host, LOGGER_COMPONENT.getName());
			break;
		default:
			break;
		}			
	}
	
	@Override
	public synchronized void incomingEvent(ServiceEvent<?> event) {
		super.incomingEvent(event);
		if(event instanceof PropertiesEvent && ((PropertiesEvent)event).getName().equals(AvailableProperties.SELENIUM)){
			notifyAll();
		}
	}
	
	@Override
	public void prepare(IFolder session, SessionType type)
			throws FITTESTException {
		super.prepare(session, type);
		_nbHost = new HashMap<String,Boolean>(); 
		_registry.findService(IPropertiesService.class).unsetProperty(AvailableProperties.SELENIUM);
	}
	

	/**
	 * Convert log from text to xml format as soon as a session is terminated
	 * 
	 */
	@Override
	public void postProcess(final IFolder session, SessionType type) {
		super.postProcess(session, type);
		
		Job convertingJob = new Job("Processing log:"){
			
			protected IStatus run(IProgressMonitor monitor) {
				
				try {
					Log2XMLBatchConverter converter = new Log2XMLBatchConverter();
					monitor.beginTask("converting...", 3);
					
					converter.recursiveConvert(session);
					monitor.worked(2);
					
					session.refreshLocal(IResource.DEPTH_INFINITE, null);
					monitor.done();
					
				} catch (CoreException e) {
					e.printStackTrace();
				}
				return new Status(IStatus.OK, "FITTEST tranformation plugin", 
							IStatus.OK, "Finish inferring model from logs!", null);
				
			}
		};
		
		convertingJob.schedule();
		
	}

	@Override
	public void upload(final Host host) throws FITTESTException { 
		if (_uploadPeriods == null) {
			_uploadPeriods = new HashMap<Host,Integer>();
		}
		Integer tmp = _uploadPeriods.get(host);
		if (tmp == null) {
			tmp = new Integer(0);
		}
		else {
			if (tmp.intValue() == Integer.MAX_VALUE) {
				tmp = new Integer(0);
			}
			else { 
				tmp = new Integer(tmp.intValue() + 1);
			}
		}
		final Integer period = tmp;
		_uploadPeriods.put(host,period);
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			// Make a non blocking call
			ExecutorService executor = Executors.newSingleThreadExecutor();
	        Future<String> future = executor.submit(new Callable<String>(){

				@Override
				public String call() throws Exception {
					Logger.getAnonymousLogger().log(Level.INFO, "Uploading periodic logs (period=" + period.intValue() + ") for host: " + host.getDescription() + " [" + host.toString() + "]");
					sendMessage(FITTESTSingleton.getObjectFactory().createUpload(), host, LOGGER_COMPONENT.getName());
					return "done";
				}
	        	
	        });

	        try {
	            String result = future.get(5, TimeUnit.SECONDS); // 5 seconds 
	            if (!"done".equals(result)){
	            	host.setCurrentLifeState(HostStates.UPLOADING_FAILED);
	            }
	            executor.shutdownNow();
	        } catch (TimeoutException e) {
	        	Logger.getAnonymousLogger().log(Level.WARNING, "Failed to get logs (period=" + period.intValue() + ") for host: " + host.getDescription() + " [" + host.toString() + "]");
	        } catch (InterruptedException e) {
				throw new FITTESTException(e.getMessage());
			} catch (ExecutionException e) {
				throw new FITTESTException(e.getMessage());
			}
			
			break;
		default:
			break;
		}
	}
	
	/**
	 * Command the host to start logging
	 *
	 *  
	 */
	@Override
	public void beginUploadSession(Host host) throws FITTESTException {
		try {
			
			// First time call when starting host, this should be null
			if(ComponentDeployment.getInstance().findComponent(host.getName(), LOGGER_COMPONENT.getName())!=null){
				
				// send to the logger component the logging parameter
				Initialize initialize = FITTESTSingleton.getObjectFactory().createInitialize();
				initialize.getParameter().addAll(_page.getInitializationParameters());
				sendMessage(initialize, host, LOGGER_COMPONENT.getName());//message won't be sent if component does not exist
				
				// tell the logger to start logging
				sendMessage(FITTESTSingleton.getObjectFactory().createStart(), host, LOGGER_COMPONENT.getName());
			}
			else{
				//throw new FITTESTException("Time-out: No FITTEST component connects to the ITE");
				Logger.getAnonymousLogger().log(Level.WARNING, "Logger component not ready!"); 
			}
		} catch (FITTESTException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void endUploadSession(Host host) throws FITTESTException {
		if (_uploadPeriods != null) {
			if (_uploadPeriods.get(host) != null){
				_uploadPeriods.remove(host);
			}
		}
	}

	@Override
	public SUTTechnologyType[] getSupportSUT() {
		SUTTechnologyType[] ret = {SUTTechnologyType.FLASH};
		return ret;
	}
	
}
