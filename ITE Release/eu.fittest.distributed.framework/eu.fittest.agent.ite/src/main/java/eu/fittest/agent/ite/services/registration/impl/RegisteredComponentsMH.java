package eu.fittest.agent.ite.services.registration.impl;



import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.RegisteredComponents;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class RegisteredComponentsMH extends AbstractMessageHandler<IRegistrationService> {

    
    public RegisteredComponentsMH(IRegistrationService service) {
        super(service);
    }
    
    public synchronized void onReception(Connection connection, RegisteredComponents message) throws FITTESTException {
    	for(String componentId: message.getId()){
    		getService().registerComponent(message.getFrom(), componentId);
    	}
    }

}
