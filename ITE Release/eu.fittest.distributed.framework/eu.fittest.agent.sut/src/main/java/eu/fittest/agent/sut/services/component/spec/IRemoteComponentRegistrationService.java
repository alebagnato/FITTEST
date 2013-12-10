package eu.fittest.agent.sut.services.component.spec;



import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.IRemoteService;


public interface IRemoteComponentRegistrationService extends IRemoteService {

    
    ComponentData registerComponent(Connection connection, final String componentName)  throws FITTESTException;

    
    void deregisterComponent(final String componentId) throws FITTESTException;
    
    void informITEaboutComponentRegistration(String componentId) throws FITTESTException;

}
