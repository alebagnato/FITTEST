package eu.fittest.component.services.indirection.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.RegisterResponse;
import eu.fittest.common.services.message.AbstractMessageHandler;

import eu.fittest.component.services.indirection.spec.IIndirectionService;

public class RegisterResponseMH extends AbstractMessageHandler<IIndirectionService> {
	
	protected RegisterResponseMH(IIndirectionService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, RegisterResponse message) throws FITTESTException {
		_service.registered();
	}

}
