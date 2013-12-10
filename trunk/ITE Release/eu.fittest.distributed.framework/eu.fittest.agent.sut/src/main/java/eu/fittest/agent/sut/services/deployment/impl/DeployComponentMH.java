package eu.fittest.agent.sut.services.deployment.impl;

import eu.fittest.agent.sut.services.deployment.spec.IDeploymentService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.DeployComponent;
import eu.fittest.common.services.message.AbstractMessageHandler;

public class DeployComponentMH extends AbstractMessageHandler<IDeploymentService>{

	protected DeployComponentMH(IDeploymentService service) {
		super(service);
	}
	

	public synchronized void onReception(Connection connection, DeployComponent message)	throws FITTESTException {
		getService().deploy(message.getResource());
	}
	
}
