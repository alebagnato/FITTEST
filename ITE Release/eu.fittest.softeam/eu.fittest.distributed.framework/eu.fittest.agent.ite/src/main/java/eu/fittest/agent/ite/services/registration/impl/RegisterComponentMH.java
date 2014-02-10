package eu.fittest.agent.ite.services.registration.impl;



import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.RegisterComponent;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class RegisterComponentMH extends AbstractMessageHandler<IRegistrationService> {

    
    public RegisterComponentMH(IRegistrationService service) {
        super(service);
    }
    
    public synchronized void onReception(Connection connection, RegisterComponent message) throws FITTESTException {
    	getService().registerComponent(message.getFrom(), message.getFittestComponentId());
    }

}
