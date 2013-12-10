package eu.fittest.component.services.indirection.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Start;
import eu.fittest.common.services.message.AbstractMessageHandler;

import eu.fittest.component.services.indirection.spec.IIndirectionService;

public class StartMH extends AbstractMessageHandler<IIndirectionService>{

	protected StartMH(IIndirectionService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, Start message)	throws FITTESTException {
		_service.start();
	}

}
