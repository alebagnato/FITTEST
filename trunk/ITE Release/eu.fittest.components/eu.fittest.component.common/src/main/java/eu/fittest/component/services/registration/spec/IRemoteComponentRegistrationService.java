package eu.fittest.component.services.registration.spec;

import eu.fittest.common.core.service.IRemoteService;

public interface IRemoteComponentRegistrationService extends IRemoteService{
	void register(String iteId, String agentId, String componentId, String componentDir);
}
