package eu.fittest.common.services.forward.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.UploadResource;
import eu.fittest.common.services.forward.spec.IForwardService;
import eu.fittest.common.services.message.AbstractMessageHandler;

public class UploadResourceMH extends AbstractMessageHandler<IForwardService>{

	protected UploadResourceMH(IForwardService service) {
		super(service);
	}
	

	public synchronized void onReception(Connection connection, UploadResource message)	throws FITTESTException {
		getService().forwardUpload(connection, message);
	}
	
}
