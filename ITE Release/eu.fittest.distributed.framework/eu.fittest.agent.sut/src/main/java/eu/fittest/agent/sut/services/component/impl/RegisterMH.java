package eu.fittest.agent.sut.services.component.impl;



import eu.fittest.agent.sut.services.component.spec.ComponentData;
import eu.fittest.agent.sut.services.component.spec.IComponentRegistrationService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Register;
import eu.fittest.common.core.xml.RegisterResponse;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class RegisterMH extends AbstractMessageHandler<IComponentRegistrationService> {

    
    public RegisterMH(IComponentRegistrationService service) {
        super(service);       
    }

    public synchronized void onReception(Connection connection, Register message)	throws FITTESTException {
    	ComponentData data =  getService().registerComponent(connection, message.getFittestEntityName());
    	RegisterResponse response = FITTESTSingleton.getObjectFactory().createRegisterResponse();
    	response.setFittestAgentId(data.getFittestAgentId());
    	response.setFittestComponentDir(data.getFittestComponentDir());
    	response.setFittestComponentId(data.getFittestComponentId());
    	response.setFittestIteId(data.getFittestIteId());
    	response.setFrom(data.getFittestAgentId());
    	response.setTo(data.getFittestComponentId());
    	connection.sendMessage(response);//the RegisterResponse message must be received prior to any other messages
    	
        getService().informITEaboutComponentRegistration(data.getFittestComponentId());
    }
    
	@Override
	public void setIdentityService(IIdentityService identity) {
		//don't consider identity
	}
}
