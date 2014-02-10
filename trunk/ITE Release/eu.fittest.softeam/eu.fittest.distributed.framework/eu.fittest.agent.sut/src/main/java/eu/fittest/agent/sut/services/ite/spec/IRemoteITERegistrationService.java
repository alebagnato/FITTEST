package eu.fittest.agent.sut.services.ite.spec;


import eu.fittest.common.core.service.IRemoteService;
import eu.fittest.common.services.registration.spec.RegistrationData;

public interface IRemoteITERegistrationService extends IRemoteService {

    
    void registeredAs(final RegistrationData data);

}
