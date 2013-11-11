package eu.fittest.agent.ite.services.registration.spec;


import eu.fittest.common.core.service.IRemoteService;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.services.registration.spec.RegistrationData;


public interface IRemoteRegistrationService extends IRemoteService {

    
    RegistrationData registerAgent(final String publicIP, final int publicPort, HUTEnvironment environment, HUTType type, String description, String oldId);

    
    void registerComponent(final String agentID, final String componentID);

    
    void deregister(final String id);

}
