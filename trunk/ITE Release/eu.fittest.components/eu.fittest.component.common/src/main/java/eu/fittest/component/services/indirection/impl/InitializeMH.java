package eu.fittest.component.services.indirection.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.services.message.AbstractMessageHandler;

import eu.fittest.component.services.indirection.spec.IIndirectionService;

public class InitializeMH extends AbstractMessageHandler<IIndirectionService>{

	protected InitializeMH(IIndirectionService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, Initialize message)	throws FITTESTException {
		_service.initialize(message.getParameter());
	}

}
