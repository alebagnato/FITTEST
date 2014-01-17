package eu.fittest.agent.ite.services.registration.impl;



import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Deregister;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class DeregisterMH extends AbstractMessageHandler<IRegistrationService> {

    
    public DeregisterMH(IRegistrationService service) {
        super(service);
    }

    
    public synchronized void onReception(final Connection connection, final Deregister message) throws FITTESTException {
        getService().deregister(message.getId());
    }

}
