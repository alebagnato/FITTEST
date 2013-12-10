package eu.fittest.agent.ite.services.agentcommand.impl;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;
import java.util.Vector;

import eu.fittest.agent.ite.services.agentcommand.spec.CommandEvent;
import eu.fittest.agent.ite.services.agentcommand.spec.IAgentCommandService;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.xml.Kill;

public class AgentCommandServiceImpl extends AbstractService implements IAgentCommandService{
	private Map<String, Vector<String>> _runningCommand;
	
	public AgentCommandServiceImpl(){
		_handlers.add(new ExecuteResponseMH(this));
		_runningCommand = new Hashtable<String, Vector<String>>();
	}
	
	
	public String getName() {
		return IAgentCommandService.class.getName();
	}

	
	public synchronized void registerRunningCommand(String agentId, String processId) {
		if(_runningCommand.get(agentId)==null) _runningCommand.put(agentId, new Vector<String>());
		_runningCommand.get(agentId).add(processId);
		fireEvent(new CommandEvent(this, agentId, processId));
	}

	public synchronized void killCommand(String agentId, String processId) throws FITTESTException {
		Vector<String> processes = _runningCommand.get(agentId);
		
		Kill kill = FITTESTSingleton.getObjectFactory().createKill();
		kill.setTaskID(processId);
		kill.setTo(agentId);
		kill.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());	
		_registry.findService(IConnectionService.class).sendMessage(kill);		
		processes.remove(processId);
		if(processes.size()==0) _runningCommand.remove(agentId);
	}

	
	public synchronized Map<String, Vector<String>> getAllRunningCommands() {
		return Collections.unmodifiableMap(_runningCommand);
	}

}
