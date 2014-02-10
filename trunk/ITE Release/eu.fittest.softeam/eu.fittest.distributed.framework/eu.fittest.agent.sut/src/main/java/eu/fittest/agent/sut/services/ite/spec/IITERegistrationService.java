package eu.fittest.agent.sut.services.ite.spec;



import eu.fittest.common.core.connection.spec.IConnectionServiceListener;
import eu.fittest.common.core.service.IService;


public interface IITERegistrationService extends IRemoteITERegistrationService, IService, ILocalITERegistrationService, IConnectionServiceListener {

}
