package eu.fittest.common.core.server.spec;


import eu.fittest.common.core.service.ILocalService;


public interface ILocalServerService extends ILocalService {

    
    void addServiceListener(final IServerServiceListener listener);

    
    void removeServiceListener(final IServerServiceListener listener);

    
    void start();

    
    void stop();
    
    Integer getServerPort();

}
