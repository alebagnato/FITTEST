package eu.fittest.common.services.forward.spec;



import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Message;
import eu.fittest.common.core.xml.UploadResource;


public interface IRemoteForwardService {

    
    void forward(Connection fromConnection, final Message message) throws FITTESTException ;
    
    void forwardUpload(Connection fromConnection, final UploadResource message) throws FITTESTException;

}
