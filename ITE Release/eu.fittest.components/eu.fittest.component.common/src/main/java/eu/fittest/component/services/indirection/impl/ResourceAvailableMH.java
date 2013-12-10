package eu.fittest.component.services.indirection.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.ResourceAvailable;
import eu.fittest.common.services.message.AbstractMessageHandler;
import eu.fittest.component.services.indirection.spec.IIndirectionService;

public class ResourceAvailableMH extends AbstractMessageHandler<IIndirectionService>{

	protected ResourceAvailableMH(IIndirectionService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, ResourceAvailable message)	throws FITTESTException {
		_service.resourceAvailable(message.getComingFrom(), message.getResource());
	}

}
