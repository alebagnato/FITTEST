package eu.fittest.agent.sut.services.deployment.impl;


import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import eu.fittest.agent.sut.services.deployment.spec.IDeploymentService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.server.spec.IServerService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.services.command.spec.ICommandService;
import eu.fittest.common.services.compression.spec.ICompressionService;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;

public class DeploymentServiceImpl extends AbstractService implements IDeploymentService {

	public DeploymentServiceImpl() {
		super();
		_handlers.add(new DeployComponentMH(this));
	}
	
    
    public void deploy(String resource) throws FITTESTException {
    	try {
			unzip(resource);
			String basePath = _registry.findService(IFileTransferService.class).getBasePath();
			String componentName = resource.substring(0,resource.lastIndexOf(".zip"));
			URI componentFolderURI = new URI(basePath+"/"+FITTESTConstants.DEFAULT_FITTEST_AGENT_COMPONENT_DEPLOYMENT_BASEDIR+"/"+componentName);
			Integer port = _registry.findService(IServerService.class).getServerPort();
			//_registry.findService(ICommandService.class).execute("java -jar "+componentName+".jar "+port.toString(), null,null,componentFolderURI);
			// begin by urueda (prevent OutOfMemory exceptions)
			if (componentName.contains("eu.fittest.component.junit")) { // enable to run lots of test cases
				final int javaHeap = 2048; // should be enough with java default
				_registry.findService(ICommandService.class).execute("java -Xmx"+javaHeap+"M -jar "+componentName+".jar "+port.toString(), null,null,componentFolderURI);
			}
			else { // use default java heap size
				_registry.findService(ICommandService.class).execute("java -jar "+componentName+".jar "+port.toString(), null,null,componentFolderURI);				
			}
			// end by urueda
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		} catch (URISyntaxException e) {
			throw new FITTESTException(e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}
    }

    
    public String getName() {
        return IDeploymentService.class.getName();
    }
    
    private void unzip(String resource) throws URISyntaxException, FileNotFoundException, IOException, FITTESTException{	
    	String basePath = _registry.findService(IFileTransferService.class).getBasePath();
		File unzipfolder = new File(new URI(basePath+"/"+FITTESTConstants.DEFAULT_FITTEST_AGENT_COMPONENT_DEPLOYMENT_BASEDIR));
		ICompressionService compression = _registry.findService(ICompressionService.class);
		compression.unzip(new File(new URI(unzipfolder.toURI().toString()+"/"+resource)), unzipfolder.getAbsolutePath());
	}

}
