package eu.fittest.common.core.service;




public interface IServiceListener<T extends ServiceEvent<? extends ILocalService>>{

    
    void incomingEvent(final T event);

}
