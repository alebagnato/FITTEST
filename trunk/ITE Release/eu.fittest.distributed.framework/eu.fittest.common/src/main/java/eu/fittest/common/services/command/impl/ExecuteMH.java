package eu.fittest.common.services.command.impl;


import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Execute;
import eu.fittest.common.core.xml.ExecuteResponse;
import eu.fittest.common.services.command.spec.ICommandService;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class ExecuteMH extends AbstractMessageHandler<ICommandService> {

    
    protected ExecuteMH(final ICommandService service) {
        super(service);
    }

    
    public synchronized void onReception(final Connection connection, final Execute message) throws FITTESTException {
        String id = getService().execute(message.getCommand(), null,null,null);
        ExecuteResponse response = FITTESTSingleton.getObjectFactory().createExecuteResponse();
        response.setTaskID(id);
        reply(connection, message, response);
    }

}
