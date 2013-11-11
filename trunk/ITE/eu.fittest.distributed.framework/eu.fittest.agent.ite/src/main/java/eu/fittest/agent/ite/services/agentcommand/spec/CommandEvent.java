package eu.fittest.agent.ite.services.agentcommand.spec;

import eu.fittest.common.core.service.ServiceEvent;

public class CommandEvent extends ServiceEvent<ILocalAgentCommandService>{
	private String _agentId;
	private String _processId;

	public String getAgentId() {
		return _agentId;
	}

	public String getProcessId() {
		return _processId;
	}

	public CommandEvent(ILocalAgentCommandService source, String agentId, String processId) {
		super(source);
		_agentId = agentId;
		_processId = processId;
	}

}
