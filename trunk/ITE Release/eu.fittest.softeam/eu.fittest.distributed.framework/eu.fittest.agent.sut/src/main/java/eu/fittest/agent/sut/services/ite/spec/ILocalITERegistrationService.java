package eu.fittest.agent.sut.services.ite.spec;



import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.ILocalService;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;


public interface ILocalITERegistrationService extends ILocalService {
    
    String getITEId();    
    
    void renewLease()  throws FITTESTException;
    
    void register(HUTEnvironment environment, HUTType type, String description) throws FITTESTException;
    
    boolean isRegistered();
    
    public void start();
    
    public void stop();

}
