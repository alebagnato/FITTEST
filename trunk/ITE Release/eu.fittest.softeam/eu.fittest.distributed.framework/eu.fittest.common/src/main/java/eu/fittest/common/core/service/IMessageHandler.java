package eu.fittest.common.core.service;


import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.xml.Message;


public interface IMessageHandler {

    
    void onReception(final Connection connection, final Message message) throws FITTESTException;

    
    void afterSending(final Connection connection, final Message message) throws FITTESTException;

    
    IService getService();
    
    void setIdentityService(IIdentityService identity);

}
