package eu.fittest.component.services.registration.impl;


import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.identity.spec.FITTESTComponent;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.component.services.registration.spec.IComponentRegistrationService;

public class ComponentRegistrationServiceImpl extends AbstractService implements IComponentRegistrationService {
	private String _iteId;
	private String _agentId;
	private String _componentDir;
	
	public ComponentRegistrationServiceImpl() {
		_iteId = null;
		_agentId = null;
		_componentDir = null;
		_handlers.add(new RegisterResponseMH(this));
	}
	
	public String getIteId() {
		return _iteId;
	}

	public String getAgentId() {
		return _agentId;
	}

	public String getComponentDir() {
		return _componentDir;
	}
	
	@Override
	public String getName() {
		return IComponentRegistrationService.class.getName();
	}

	@Override
	public void register(String iteId, String agentId, String componentId,	String componentDir) {
		IConnectionService connectionService =  _registry.findService(IConnectionService.class);
		if(_iteId!=null){
			connectionService.unmap(_iteId);
		}
		if(_agentId!=null){
			connectionService.unmap(_agentId);
		}
		_iteId = iteId;
		_agentId = agentId;
		_componentDir = componentDir;
		
		_registry.findService(IIdentityService.class).setMyIdentity(new FITTESTComponent(componentId));
	    connectionService.map(_agentId, connectionService.getConnection("127.0.0.1",
	    		new Integer(System.getProperty(FITTESTConstants.FITTEST_AGENT_PORT))));
	}

}
