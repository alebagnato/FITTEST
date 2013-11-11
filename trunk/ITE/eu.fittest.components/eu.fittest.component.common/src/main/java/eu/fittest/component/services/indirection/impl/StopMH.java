package eu.fittest.component.services.indirection.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Stop;
import eu.fittest.common.services.message.AbstractMessageHandler;

import eu.fittest.component.services.indirection.spec.IIndirectionService;

public class StopMH extends AbstractMessageHandler<IIndirectionService>{

	protected StopMH(IIndirectionService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, Stop message)	throws FITTESTException {
		_service.stop();
	}

}
