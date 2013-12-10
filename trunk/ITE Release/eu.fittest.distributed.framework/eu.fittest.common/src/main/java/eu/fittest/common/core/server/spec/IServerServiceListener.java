package eu.fittest.common.core.server.spec;


import eu.fittest.common.core.service.IServiceListener;


public interface IServerServiceListener extends IServiceListener {

    
    void incomingEvent(final ServerEvent event);

}
