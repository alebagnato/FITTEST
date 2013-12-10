package eu.fittest.agent.sut.services.serialization.spec;



import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.IRemoteService;


public interface IRemoteSerializationService extends IRemoteService {

    
    String startSerialization(final String resource) throws FITTESTException ;

    
    void stopSerialization(final String resource) throws FITTESTException ;

}
