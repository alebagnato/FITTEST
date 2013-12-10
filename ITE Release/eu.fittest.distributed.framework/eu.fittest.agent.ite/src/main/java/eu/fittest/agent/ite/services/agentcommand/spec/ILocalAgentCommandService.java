package eu.fittest.agent.ite.services.agentcommand.spec;

import java.util.Map;
import java.util.Vector;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.ILocalService;

public interface ILocalAgentCommandService extends ILocalService{
	Map<String, Vector<String>> getAllRunningCommands();
	void killCommand(String agentId, String processId) throws FITTESTException;
}
