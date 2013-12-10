package eu.fittest.common.core.service;

import eu.fittest.common.core.identity.spec.IIdentityService;

public abstract class AbstractService extends AbstractLocalService implements IService{

	protected IServiceRegistry _registry;

	protected AbstractService() {
		_registry = null;
	}
	
	
	public void setRegistry(IServiceRegistry registry) {
		_registry = registry;
		for(IMessageHandler h: _handlers){
			h.setIdentityService(_registry.findService(IIdentityService.class));
		}
	}

}
