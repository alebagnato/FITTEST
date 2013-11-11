package eu.fittest.common.services.forward.impl;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Message;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.core.xml.UploadResource;
import eu.fittest.common.services.compression.spec.ICompressionService;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.services.forward.spec.ForwardServiceEvent;
import eu.fittest.common.services.forward.spec.IForwardService;
import eu.fittest.common.util.Validation;


public class ForwardServiceImpl extends AbstractService implements IForwardService {

	public ForwardServiceImpl() {
		super();
		_handlers.add(new MessageMH(this));
		_handlers.add(new UploadResourceMH(this));
	}
	
	public synchronized void forward(Connection fromConnection, Message message) throws FITTESTException {
    	ITELogger.log(Level.FINEST, "Trying to forward to "+message.getTo());
    	if(message.getTo()!=null && !_registry.findService(IIdentityService.class).getMyIdentity().equals(message.getTo())){
    		IConnectionService connectionService =_registry.findService(IConnectionService.class); 
    		if(connectionService.getConnection(message.getTo())!=null){
    			connectionService.sendMessage(message);
    		}
    		else{
    			_registry.findService(IConnectionService.class).broadcastMessage(fromConnection, message);
    		}
    		ITELogger.log(Level.INFO, "Forwarding "+message.getClass().getSimpleName()+" to "+message.getTo());
        	fireEvent(new ForwardServiceEvent(this, message));
    	}
    }
    
    public synchronized void forwardUpload(Connection fromConnection, UploadResource message) throws FITTESTException{
		Upload upload = FITTESTSingleton.getObjectFactory().createUpload();
		upload.setFrom(message.getTo());
		upload.setTo(message.getTowards());
		try {
			URI fileURI = new URI(message.getResource());
			File file = null;
			if(fileURI.isAbsolute()){
				file = new File(fileURI);
			}
			else{
				file = new File(new URI(_registry.findService(IFileTransferService.class).getBasePath()));
				file = new File(file, message.getResource());
			}
			if(file.exists()){
				if(file.isDirectory()){
					ICompressionService compression = _registry.findService(ICompressionService.class);
					upload.setInflate(true);
					file = compression.zipAndGzip(file);
					upload.setChecksum(compression.checksum(file));
				}			
				
				upload.setResourceSize(file.length());
				upload.setResource(message.getTo()+"/"+Validation.formatToValidFileName(message.getFrom())+"/"+file.getName());//forward slash because it is a URI
				forward(fromConnection, upload);
		    	
				Connection connection = _registry.findService(IConnectionService.class).getConnection(message.getTowards());
		    	_registry.findService(IFileTransferService.class).download(connection.getOutputStream(), file.toURI().toString());	    	
			}
			else{
				throw new FITTESTException(file.getAbsolutePath()+ " does not exist");
			}
		} catch (URISyntaxException e) {
			throw new FITTESTException(e.getMessage());
		}
		
    }
  
  
    
    public String getName() {
        return IForwardService.class.getName();
    }

}
