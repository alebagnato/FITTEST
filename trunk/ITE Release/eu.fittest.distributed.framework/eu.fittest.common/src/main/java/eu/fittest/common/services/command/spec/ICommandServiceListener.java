package eu.fittest.common.services.command.spec;


import eu.fittest.common.core.service.IServiceListener;


public interface ICommandServiceListener extends IServiceListener {

    
    void incomingEvent(final CommandEvent event);

}
