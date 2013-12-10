package eu.fittest.agent.ite.services.agentcommand.spec;

import eu.fittest.common.core.service.IRemoteService;

public interface IRemoteAgentCommandService extends IRemoteService{
	void registerRunningCommand(String agentId, String processId);
}
