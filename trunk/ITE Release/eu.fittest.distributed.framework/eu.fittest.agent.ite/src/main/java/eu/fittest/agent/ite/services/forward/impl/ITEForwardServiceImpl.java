package eu.fittest.agent.ite.services.forward.impl;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Iterator;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.ResourceAvailable;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.core.xml.UploadResource;
import eu.fittest.common.services.compression.spec.ICompressionService;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;


public class ITEForwardServiceImpl extends eu.fittest.common.services.forward.impl.ForwardServiceImpl {

	public ITEForwardServiceImpl() {
		super();
	}
    
	private String findAgent(String id){
		String result=null;
		Iterator<String> itAgents = _registry.findService(IRegistrationService.class).getAllAgents().iterator();
		while(result==null && itAgents.hasNext()){
			String currentAgent = itAgents.next();
			if(currentAgent.equals(id)){
				result = currentAgent;
			}
			else{
				Iterator<String> itComponents = _registry.findService(IRegistrationService.class).getAllComponentsOnAgent(currentAgent).iterator();
				while(result==null && itComponents.hasNext()){
					String currentComponent = itComponents.next();
					if(currentComponent.equals(id)){
						result = currentAgent;
					}
				}
			}
		}
		return result;
	}
	
    public void forwardUpload(Connection fromConnection, UploadResource message) throws FITTESTException{
    	Upload upload = FITTESTSingleton.getObjectFactory().createUpload();
		upload.setFrom(message.getTo());
		
		String agent = findAgent(message.getTowards());		
		upload.setTo(agent);
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
				upload.setResource(message.getTowards()+"/"+file.getName());//forward slash because it is a URI
				forward(fromConnection, upload);
		    	
				Connection connection = _registry.findService(IConnectionService.class).getConnection(message.getTowards());
		    	_registry.findService(IFileTransferService.class).download(connection.getOutputStream(), file.toURI().toString());	
		    	
		    	if(!agent.equals(message.getTowards())){
		    		ResourceAvailable resourceMessage = FITTESTSingleton.getObjectFactory().createResourceAvailable();
		    		resourceMessage.setFrom(message.getTo());
		    		resourceMessage.setTo(message.getTowards());
		    		resourceMessage.setComingFrom(message.getFrom());
		    		resourceMessage.setResource(message.getResource());
		    		_registry.findService(IConnectionService.class).sendMessage(resourceMessage);
		    	}
			}
			else{
				throw new FITTESTException(file.getAbsolutePath()+ " does not exist");
			}
		} catch (URISyntaxException e) {
			throw new FITTESTException(e.getMessage());
		}
    }

}
