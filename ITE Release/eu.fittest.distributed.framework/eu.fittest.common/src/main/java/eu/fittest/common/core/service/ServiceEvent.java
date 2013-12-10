package eu.fittest.common.core.service;




public class ServiceEvent<T extends ILocalService> {
    
    private T _source;


    
    public T getSource() {
        return _source;
    }

    
    public ServiceEvent(T source) {
        _source = source;
    }

}
