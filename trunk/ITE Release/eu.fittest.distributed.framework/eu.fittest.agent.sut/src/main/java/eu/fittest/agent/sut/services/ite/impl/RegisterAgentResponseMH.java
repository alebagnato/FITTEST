package eu.fittest.agent.sut.services.ite.impl;



import eu.fittest.agent.sut.services.ite.spec.IITERegistrationService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.FITTESTAgent;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.RegisterAgentReponse;
import eu.fittest.common.services.message.AbstractMessageHandler;
import eu.fittest.common.services.registration.spec.RegistrationData;


public class RegisterAgentResponseMH extends AbstractMessageHandler<IITERegistrationService> {

    
    public RegisterAgentResponseMH(IITERegistrationService service) {
        super(service);
    }
    
    public synchronized void onReception(Connection connection, RegisterAgentReponse message)	throws FITTESTException {
    	RegistrationData data = new RegistrationData(message.getFittestLease(), new FITTESTAgent(message.getFittestAgentId()), message.getFittestIteId());
    	getService().registeredAs(data);
    }

}
