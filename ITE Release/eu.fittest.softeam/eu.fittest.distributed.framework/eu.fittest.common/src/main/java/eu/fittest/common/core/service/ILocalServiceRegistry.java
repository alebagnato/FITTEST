package eu.fittest.common.core.service;




public interface ILocalServiceRegistry extends ILocalService {

    
    void registerService(final IService service);

    
    <T extends eu.fittest.common.core.service.IService> T findService(final Class<T> serviceName);

}
