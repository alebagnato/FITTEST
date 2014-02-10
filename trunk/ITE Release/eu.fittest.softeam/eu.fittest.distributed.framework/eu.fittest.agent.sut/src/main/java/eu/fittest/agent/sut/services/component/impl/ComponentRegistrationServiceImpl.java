package eu.fittest.agent.sut.services.component.impl;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.agent.sut.services.component.spec.ComponentData;
import eu.fittest.agent.sut.services.component.spec.ComponentRegistrationEvent;
import eu.fittest.agent.sut.services.component.spec.ComponentRegistrationEventKind;
import eu.fittest.agent.sut.services.component.spec.IComponentRegistrationService;
import eu.fittest.agent.sut.services.ite.spec.IITERegistrationService;
import eu.fittest.agent.sut.services.ite.spec.ITERegistrationEvent;
import eu.fittest.agent.sut.services.ite.spec.ITERegistrationEventKind;
import eu.fittest.common.core.connection.spec.ConnectionServiceEvent;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.FITTESTComponent;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.Deregister;
import eu.fittest.common.core.xml.RegisterComponent;
import eu.fittest.common.core.xml.RegisterResponse;
import eu.fittest.common.core.xml.RegisteredComponents;
import eu.fittest.common.util.Validation;

public class ComponentRegistrationServiceImpl extends AbstractService implements IComponentRegistrationService {
	private Map<String,ComponentData> _components;
	
	public ComponentRegistrationServiceImpl(){
		super();
		_handlers.add(new RegisterMH(this));
		_handlers.add(new DeregisterMH(this));
		_components = new HashMap<String,ComponentData>();
	}

    public synchronized ComponentData registerComponent(Connection connection, String componentName) throws FITTESTException {
    	ITELogger.log(Level.INFO, "component "+componentName+" is registered");
    	
    	ComponentData data = new ComponentData();
    	data.setFittestComponentId(new FITTESTComponent(componentName+"-"+UUID.randomUUID().toString()).getId());
        try {
        	String identityPath = System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)+_registry.findService(IIdentityService.class).getMyIdentity();
        	String struri = identityPath+"/"+Validation.formatToValidFileName(data.getFittestComponentId());
    		File file = new File(new URI(struri));
    		if(! file.exists()){
    			if(!file.mkdirs()){
    				throw new FITTESTException("Can't create path to "+struri+" on local file system");
    			}
    		}
    		data.setFittestComponentDir(file.toURI().toString());
		} catch (URISyntaxException e) {
			ITELogger.log(Level.SEVERE, e.getMessage());
			throw new FITTESTException(e.getMessage());
		}
    	data.setFittestAgentId(_registry.findService(IIdentityService.class).getMyIdentity());
        data.setFittestIteId(_registry.findService(IITERegistrationService.class).getITEId());
        _registry.findService(IConnectionService.class).map(data.getFittestComponentId(), connection);
        
        _components.put(data.getFittestComponentId(),data);
        
        fireEvent(new ComponentRegistrationEvent(this, data, ComponentRegistrationEventKind.registration));       
    	return data;
    }
    
    private void informITEaboutAllRegisteredComponents() throws FITTESTException{
    	RegisteredComponents registeredComponents = FITTESTSingleton.getObjectFactory().createRegisteredComponents();
    	registeredComponents.getId().addAll(_components.keySet());
    	registeredComponents.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
    	registeredComponents.setTo(_registry.findService(IITERegistrationService.class).getITEId());
    	_registry.findService(IConnectionService.class).sendMessage(registeredComponents);
    }
    
    private void informComponentsAboutITE() throws FITTESTException{
		String newITEId = _registry.findService(IITERegistrationService.class).getITEId();
    	for(String component: _components.keySet()){//for dealing with ITE new ID
    		ComponentData data = _components.get(component);
    		if(!data.getFittestIteId().equals(newITEId)){
    			data.setFittestIteId(newITEId);
    			data.setFittestAgentId(_registry.findService(IIdentityService.class).getMyIdentity());
    			
		    	RegisterResponse registerresponse = FITTESTSingleton.getObjectFactory().createRegisterResponse();
		    	registerresponse.setFrom(data.getFittestAgentId());
		    	registerresponse.setFittestAgentId(data.getFittestAgentId());
		    	
		    	registerresponse.setTo(component);
		    	registerresponse.setFittestComponentId(component);
		    	
		    	registerresponse.setFittestIteId(newITEId);
		    	registerresponse.setFittestComponentDir(data.getFittestComponentDir());
		    	_registry.findService(IConnectionService.class).sendMessage(registerresponse);
    		}
    	}
    }
    
    public synchronized void informITEaboutComponentRegistration(String componentId) throws FITTESTException{
    	RegisterComponent register = FITTESTSingleton.getObjectFactory().createRegisterComponent();
        register.setTo(_registry.findService(IITERegistrationService.class).getITEId());
        register.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
        register.setFittestComponentId(componentId);
        _registry.findService(IConnectionService.class).sendMessage(register);
    }

    
    public synchronized void deregisterComponent(String componentId) throws FITTESTException {
    	ITELogger.log(Level.INFO, "component "+componentId+" is deregistered");
    	Deregister deregister = FITTESTSingleton.getObjectFactory().createDeregister();
    	deregister.setTo(_registry.findService(IITERegistrationService.class).getITEId());
    	deregister.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
    	deregister.setId(componentId);
        _registry.findService(IConnectionService.class).sendMessage(deregister);
        
        ComponentData data =new ComponentData();
        data.setFittestComponentId(componentId);
        
        _components.remove(data.getFittestComponentId());
        
        fireEvent(new ComponentRegistrationEvent(this, data, ComponentRegistrationEventKind.deregistration));
    }

	
	public String getName() {
		return IComponentRegistrationService.class.getName();
	}

	public synchronized void incomingEvent(ConnectionServiceEvent event) {
		switch(event.getKind()){
		case add:
			//nothing to do
			break;
		case remove:
			List<String> ids = event.getSource().getReachableId(event.getConnection());
			for(String id: ids){
				try {
					deregisterComponent(id);
				} catch (FITTESTException e) {
					ITELogger.log(Level.SEVERE, e.getMessage());
				}
			}
			break;
		}
	}
	
	public synchronized void incomingEvent(ITERegistrationEvent event){
		if(event.getKind().equals(ITERegistrationEventKind.Registered)){
			try {
				informITEaboutAllRegisteredComponents();
				informComponentsAboutITE();
			} catch (FITTESTException e) {
				ITELogger.log(Level.SEVERE, e.getMessage());
			}
		}
	}

	
	public synchronized void incomingEvent(ServiceEvent event) {	
		if(event instanceof ConnectionServiceEvent){
			incomingEvent((ConnectionServiceEvent)event);
		}
		else if(event instanceof ITERegistrationEvent){
			incomingEvent((ITERegistrationEvent)event);
		}
	}

}
