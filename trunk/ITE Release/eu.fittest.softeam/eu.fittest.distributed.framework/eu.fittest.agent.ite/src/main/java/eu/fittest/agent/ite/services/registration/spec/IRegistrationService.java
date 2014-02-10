package eu.fittest.agent.ite.services.registration.spec;



import eu.fittest.common.core.connection.spec.IConnectionServiceListener;
import eu.fittest.common.core.service.IService;


public interface IRegistrationService extends IService, ILocalRegistrationService, IRemoteRegistrationService, IConnectionServiceListener {

}
