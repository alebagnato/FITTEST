package eu.fittest.agent.sut.services.forward.impl;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;

import eu.fittest.agent.sut.services.ite.spec.IITERegistrationService;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.core.xml.UploadResource;
import eu.fittest.common.services.compression.spec.ICompressionService;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.util.Validation;


public class AgentForwardServiceImpl extends eu.fittest.common.services.forward.impl.ForwardServiceImpl {

	public AgentForwardServiceImpl() {
		super();
	}
    
    public void forwardUpload(Connection fromConnection, UploadResource message) throws FITTESTException{
    	Connection connection = _registry.findService(IConnectionService.class).getConnection(message.getTowards());
    	String iteID = _registry.findService(IITERegistrationService.class).getITEId();
    	if(connection !=null && connection.equals(_registry.findService(IConnectionService.class).getConnection(iteID))){
    		super.forwardUpload(fromConnection, message);
    	}
    	else{
			Upload upload = FITTESTSingleton.getObjectFactory().createUpload();
			upload.setFrom(message.getTo());
			upload.setTo(iteID);
			try {
				File file = new File(new URI(message.getResource()));
				String resourceName = file.getName();
				if(file.isDirectory()){
					ICompressionService compression = _registry.findService(ICompressionService.class);
					upload.setInflate(true);
					file = compression.zipAndGzip(file);
					upload.setChecksum(compression.checksum(file));
				}			
				
				upload.setResourceSize(file.length());
				upload.setResource(message.getTo()+"/"+Validation.formatToValidFileName(message.getFrom())+"/"+file.getName());//forward slash because it is a URI
				forward(fromConnection, upload);
		    	
				
		    	_registry.findService(IFileTransferService.class).download(
		    			_registry.findService(IConnectionService.class).getConnection(upload.getTo()).getOutputStream(), file.toURI().toString());
		    	
		    	UploadResource messagetoITE = FITTESTSingleton.getObjectFactory().createUploadResource();
		    	messagetoITE.setTo(upload.getTo());
		    	messagetoITE.setFrom(message.getFrom());
		    	messagetoITE.setTowards(message.getTowards());
		    	messagetoITE.setResource(message.getTo()+"/"+Validation.formatToValidFileName(message.getFrom())+"/"+resourceName);
		    	_registry.findService(IConnectionService.class).sendMessage(messagetoITE);
		    	
			} catch (URISyntaxException e) {
				throw new FITTESTException(e.getMessage());
			}
    	}
    }

}
