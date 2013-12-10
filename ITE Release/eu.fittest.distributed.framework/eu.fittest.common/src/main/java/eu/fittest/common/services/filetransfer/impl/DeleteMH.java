package eu.fittest.common.services.filetransfer.impl;


import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Delete;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class DeleteMH extends AbstractMessageHandler<IFileTransferService> {

    
    public DeleteMH(IFileTransferService service) {
        super(service);
    }

    
    public synchronized void onReception(Connection connection, Delete message) throws FITTESTException {
        getService().delete(message.getResource());
    }

}
