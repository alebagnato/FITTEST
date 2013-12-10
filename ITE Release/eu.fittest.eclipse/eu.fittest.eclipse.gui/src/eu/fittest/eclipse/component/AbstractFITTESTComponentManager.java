package eu.fittest.eclipse.component;

import org.eclipse.core.resources.IFolder;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.agent.ite.services.registration.spec.RegistrationEvent;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceListener;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.Message;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.model.constants.FITTESTEclipseConstants;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.ComponentDeployment;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.project.config.SUTTechnologyType;
import eu.fittest.project.config.TestProject;

// TODO: Auto-generated Javadoc
/**
 * The Class AbstractFITTESTComponentManager.
 */
public abstract class AbstractFITTESTComponentManager implements IFITTESTComponentManager, IServiceListener<ServiceEvent<?>>{
	

	protected ComponentInformation[] _components;
	
	/** The _registry. */
	protected IServiceRegistry _registry;
	
	protected IFolder _sessionFolder;
	
	protected SessionType _type;
	
	/**
	 * Instantiates a new abstract fittest component manager.
	 *
	 * @param componentName the component name
	 * @param componentLocation the component location
	 * @param pluginID the plugin id
	 */
	protected AbstractFITTESTComponentManager(ComponentInformation info){
		this(new ComponentInformation[]{info});
	}
	
	protected AbstractFITTESTComponentManager(ComponentInformation[] componentInformations){
		_components = componentInformations;
	}
	
	/**
	 * Deployment condition.
	 *
	 * @param host the host
	 * @return true, if successful
	 */
	abstract protected boolean deploymentCondition(Host host, String componentName);
	
	/* (non-Javadoc)
	 * @see eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager#setServiceRegistry(eu.fittest.common.core.service.IServiceRegistry)
	 */
	@Override
	public void setServiceRegistry(IServiceRegistry registry) {
		if(_registry!=null){
			_registry.findService(IRegistrationService.class).removeServiceListener(this);
		}
		_registry = registry;
		if(_registry!=null){
			registry.findService(IRegistrationService.class).addServiceListener(this);
		}
	}
	
	/* (non-Javadoc)
	 * @see eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager#deployOn(eu.fittest.eclipse.model.environment.Host)
	 */
	public void deployOn(Host host) throws FITTESTException{
		for(ComponentInformation info: _components){
			if(deploymentCondition(host,info.getName())){
				ComponentDeployment.getInstance().deployIfRequired(host.getName(),info.getName(), info.getPathInPlugin(), info.getPluginID());
			}
		}
	}
	
	/**
	 * Wait for deployment.
	 *
	 * @param host the host
	 * @return the string
	 * @throws FITTESTException the fITTEST exception
	 */
	synchronized protected String waitForDeployment(Host host, String componentName) throws FITTESTException{
		String componentInstance = ComponentDeployment.getInstance().findComponent(host.getName(), componentName);
		int counter = 0; // add counter to stop waiting
		while(componentInstance==null && counter < 2) { 
			counter++;
			try {
				wait(FITTESTEclipseConstants.DEFAULT_COMPONENT_CONNECTION_TIME_OUT);
			} catch (InterruptedException e) {
				throw new FITTESTException(e.getMessage());
			}
			componentInstance = ComponentDeployment.getInstance().findComponent(host.getName(), componentName);
		}
		if (componentInstance == null)
			throw new FITTESTException("Timeout waiting for component deployment on " + host.getName());
		
		return componentInstance;
	}
	
	/* (non-Javadoc)
	 * @see eu.fittest.common.core.service.IServiceListener#incomingEvent(eu.fittest.common.core.service.ServiceEvent)
	 */
	public synchronized void incomingEvent(ServiceEvent<?> event) {
		if(event instanceof RegistrationEvent){
			boolean found = false;
			int i = 0;
			while(!found && i<_components.length){
				found = ((RegistrationEvent)event).getRegistrationData().getEntity().getId().startsWith(_components[i].getName());
				i++;
			}
			if(found){
				notifyAll();
			}
		}
	}
	
	
	/**
	 * Send message only if a component is deployed on the host
	 *
	 * @param message the message
	 * @param host the host
	 * @throws FITTESTException the fITTEST exception
	 */
	protected void sendMessage(Message message, Host host, String componentTypeName) throws FITTESTException{
		String componentInstanceId = ComponentDeployment.getInstance().findComponent(host.getName(), componentTypeName);
		if(componentInstanceId!=null){//component is deployed on that host
			message.setTo(componentInstanceId);
			message.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
			_registry.findService(IConnectionService.class).sendMessage(message);
		}
	}
	
	public void prepare(IFolder session, SessionType type) throws FITTESTException {
		_sessionFolder = session;
		_type = type;
	}
	
	public void upload(Host host) throws FITTESTException {
		// override by interested components
	}
	
	
	public void beginUploadSession(Host host) throws FITTESTException {
		// concrete class should overwrite as needed
	}

	public void endUploadSession(Host host) throws FITTESTException {
		// concrete class should overwrite as needed
	}

		
	/**
	 * Post process should be call after a session
	 * 
	 * Interested component can overwrite to implement neccessary post processing tasks
	 * like log conversion, report preparation etc.
	 *  
	 */
	public void postProcess(IFolder session, SessionType type){
		_sessionFolder = session;
		_type = type;
	}
	
	/**
	 * Specified support technology, default support all, overwite as needed
	 * 
	 */
	public SUTTechnologyType[] getSupportSUT() {
		return SUTTechnologyType.values();  
	}
	
	/**
	 * Get the SUT technology of the current active project, if no selected project found
	 * return FLASH by default
	 * 
	 * @return
	 */
	protected SUTTechnologyType getSUTTechOfSelectedProject(){
		TestProject projectConf = Activator.getDefault().getActiveProjectConfig();
		if (projectConf != null){
			return projectConf.getGeneral().getType();
		}
		
		return SUTTechnologyType.FLASH;
	}
	
}
