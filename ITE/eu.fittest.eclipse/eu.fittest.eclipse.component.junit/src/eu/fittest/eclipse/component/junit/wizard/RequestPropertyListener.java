package eu.fittest.eclipse.component.junit.wizard;

import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.xml.RequestProperties;
import eu.fittest.common.core.xml.RequestProperties.Property;
import eu.fittest.component.appdescription.ite.services.properties.impl.PropertiesServiceImpl;
import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesListener;
import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesService;
import eu.fittest.component.appdescription.ite.services.properties.spec.PropertiesEvent;
import eu.fittest.eclipse.component.appdescription.wizard.AppDescriptionComponentManager;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.ComponentDeployment;

public class RequestPropertyListener implements IPropertiesListener{
	private IServiceRegistry _registry;
	
	public RequestPropertyListener(IServiceRegistry registry){
		_registry = registry;
		if(_registry.findService(IPropertiesService.class)==null){
			_registry.registerService(new PropertiesServiceImpl());
		}
	}
	
	public void requestProperty(Host h, String... propertyNames) throws FITTESTException{
		_registry.findService(IPropertiesService.class).addServiceListener(this);
		RequestProperties message = FITTESTSingleton.getObjectFactory().createRequestProperties();
		for(String name: propertyNames){
			Property p = FITTESTSingleton.getObjectFactory().createRequestPropertiesProperty();
			p.setName(name);
			message.getProperty().add(p);
			_registry.findService(IPropertiesService.class).unsetProperty(name);
		}
		message.setTo(ComponentDeployment.getInstance().findComponent(h.getName(), AppDescriptionComponentManager.APPDESCRIPTION_COMPONENT.getName()));
		message.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
		_registry.findService(IConnectionService.class).sendMessage(message);
	}
	
	public String waitForValue(String property){
		IPropertiesService service = _registry.findService(IPropertiesService.class);
		synchronized (service) {//sync on service is required when several properties are requested at the same time
			while(!service.isPropertySet(property)){
				try {
					Logger.getAnonymousLogger().log(Level.INFO, "waiting for "+property);
					service.wait();
					Logger.getAnonymousLogger().log(Level.INFO, "unwaiting for "+property);
				} catch (InterruptedException e) {
				}
			}
		}
		_registry.findService(IPropertiesService.class).removeServiceListener(this);
		return service.getProperty(property);
	}
	
	@Override
	public synchronized void incomingEvent(PropertiesEvent event) {
		Logger.getAnonymousLogger().log(Level.INFO, "Incoming requested property "+event.getName());
		_registry.findService(IPropertiesService.class).notifyAll();
	}

}
