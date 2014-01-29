package eu.fittest.agent.ite.services.agentcommand.impl;

import eu.fittest.agent.ite.services.agentcommand.spec.IAgentCommandService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.ExecuteResponse;
import eu.fittest.common.services.message.AbstractMessageHandler;

public class ExecuteResponseMH extends AbstractMessageHandler<IAgentCommandService> {

	protected ExecuteResponseMH(IAgentCommandService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, ExecuteResponse message)	throws FITTESTException {
		getService().registerRunningCommand(message.getFrom(), message.getTaskID());
	}

}
