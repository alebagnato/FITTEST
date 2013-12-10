package eu.fittest.common.services.command.impl;


import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Kill;
import eu.fittest.common.services.command.spec.ICommandService;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class AbortMH extends AbstractMessageHandler<ICommandService> {

    
    protected AbortMH(final ICommandService service) {
        super(service);
    }

    
    public synchronized void onReception(final Connection connection, final Kill message) throws FITTESTException {
        getService().kill(message.getTaskID());
    }

}
