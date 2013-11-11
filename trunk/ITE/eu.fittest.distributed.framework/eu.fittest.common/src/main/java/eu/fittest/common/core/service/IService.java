package eu.fittest.common.core.service;




public interface IService extends IRemoteService, ILocalService {

    
    String getName();

    
    void setRegistry(final IServiceRegistry registry);

}
