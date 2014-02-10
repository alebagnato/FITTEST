package eu.fittest.agent.sut.services.component.impl;



import eu.fittest.agent.sut.services.component.spec.IComponentRegistrationService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Deregister;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class DeregisterMH extends AbstractMessageHandler<IComponentRegistrationService> {

    
    public DeregisterMH(IComponentRegistrationService service) {
        super(service);
    }


    public synchronized void onReception(Connection connection, Deregister message) throws FITTESTException {
    	getService().deregisterComponent(message.getId());
    }
}
