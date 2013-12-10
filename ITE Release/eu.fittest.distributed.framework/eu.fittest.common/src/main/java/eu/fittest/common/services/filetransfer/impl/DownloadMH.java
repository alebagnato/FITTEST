package eu.fittest.common.services.filetransfer.impl;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;


import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Download;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.services.message.AbstractMessageHandler;



public class DownloadMH extends AbstractMessageHandler<IFileTransferService> {

    
    public DownloadMH(IFileTransferService service) {
        super(service);
    }

    
    public synchronized void onReception(Connection connection, Download message) {
        try {
            Upload upload = FITTESTSingleton.getObjectFactory().createUpload();
            upload.setResource(message.getResource());
            upload.setResourceSize(new File(new URI(message.getResource())).length());
            reply(connection, message, upload);
            getService().download(connection.getOutputStream(), message.getResource());
        } catch (FITTESTException e) {
            ITELogger.log(Level.SEVERE, e.getMessage());
        } catch (URISyntaxException e) {
            ITELogger.log(Level.SEVERE, e.getMessage());
        }
    }

}
