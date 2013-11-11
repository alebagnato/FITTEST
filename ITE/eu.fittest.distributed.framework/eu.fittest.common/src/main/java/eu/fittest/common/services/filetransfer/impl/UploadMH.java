package eu.fittest.common.services.filetransfer.impl;


import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.services.message.AbstractMessageHandler;

public class UploadMH extends AbstractMessageHandler<IFileTransferService> {

    
    public UploadMH(IFileTransferService service) {
        super(service);
    }

    
    public synchronized void onReception(Connection connection, Upload message) throws FITTESTException {
        getService().upload(connection.getInputStream(),message.getResource(), message.getResourceSize(), message.isInflate(), message.getChecksum());
    }

}
