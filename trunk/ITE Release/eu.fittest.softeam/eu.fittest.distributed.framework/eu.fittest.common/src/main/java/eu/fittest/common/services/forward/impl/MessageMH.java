package eu.fittest.common.services.forward.impl;



import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Message;
import eu.fittest.common.services.forward.spec.IForwardService;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class MessageMH extends AbstractMessageHandler<IForwardService> {

    
    public MessageMH(IForwardService service) {
        super(service);
    }

    
    public synchronized void onReception(Connection connection, Message message)	throws FITTESTException {
    	getService().forward(connection, message);
    }
}
