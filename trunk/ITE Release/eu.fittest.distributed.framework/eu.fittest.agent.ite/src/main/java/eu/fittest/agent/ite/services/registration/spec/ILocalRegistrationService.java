package eu.fittest.agent.ite.services.registration.spec;

import java.util.List;
import eu.fittest.common.core.service.ILocalService;
import eu.fittest.common.services.registration.spec.RegistrationData;

public interface ILocalRegistrationService extends ILocalService {

    void addRegistrationListener(final IRegistrationServiceListener listener);

    void removeRegistrationListener(final IRegistrationServiceListener listener);

    List<String> getAllAgents();

    List<String> getAllComponentsOnAgent(final String agentID);
    
    RegistrationData getAgentData(String agentId);

}
