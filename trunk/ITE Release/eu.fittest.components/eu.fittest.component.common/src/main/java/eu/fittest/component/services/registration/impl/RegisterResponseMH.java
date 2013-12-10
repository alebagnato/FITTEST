package eu.fittest.component.services.registration.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.RegisterResponse;
import eu.fittest.common.services.message.AbstractMessageHandler;
import eu.fittest.component.services.registration.spec.IComponentRegistrationService;

public class RegisterResponseMH extends AbstractMessageHandler<IComponentRegistrationService> {

	protected RegisterResponseMH(IComponentRegistrationService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, RegisterResponse message) throws FITTESTException {
		_service.register(message.getFittestIteId(), message.getFittestAgentId(), message.getFittestComponentId(), message.getFittestComponentDir());
	}
	
	@Override
	public void setIdentityService(IIdentityService identity) {
		// don't consider identity because it is set by this message
	}

}
