package eu.fittest.eclipse.model.jobs;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.xml.DeployComponent;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class ComponentDeployment {
	private IServiceRegistry _registry;
	private static ComponentDeployment _instance = null;
	
	public static ComponentDeployment getInstance() throws FITTESTException{
		synchronized(ComponentDeployment.class){
			if(_instance==null) 
				_instance = new ComponentDeployment();
			return _instance;
		}
	}
	
	private ComponentDeployment() throws FITTESTException {
		_registry = FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry();
	}

	public String findComponent(String agentId, String componentName){
		String found = null;
		Iterator<String> itComponents = _registry.findService(IRegistrationService.class).getAllComponentsOnAgent(agentId).iterator();
		while(found==null && itComponents.hasNext()){
			String current = itComponents.next();
			if(current.startsWith(componentName)){
				found = current;
			}
		}
		return found;
	}
	
	public void deployIfRequired(String agentId, String componentName, String componentBundle, String bundleId) throws FITTESTException{
		if(findComponent(agentId, componentName)==null){
			try {
				deployComponent(agentId, componentBundle, bundleId);
			} catch (URISyntaxException e) {
				throw new FITTESTException(e.getMessage());
			} catch (IOException e) {
				throw new FITTESTException(e.getMessage());
			}
		}
	}
	
	private void deployComponent(String agentId, String componentBundle, String bundleId) throws URISyntaxException, IOException, FITTESTException{
		Logger.getAnonymousLogger().log(Level.INFO,"Deploying "+componentBundle+" to "+agentId);
		Bundle bundle = Platform.getBundle(bundleId);
		Logger.getAnonymousLogger().log(Level.FINEST,"Bundle "+bundle.toString()+" is used "+FileLocator.toFileURL(bundle.getEntry(componentBundle)));
		File component = new File(FileLocator.toFileURL(bundle.getEntry(componentBundle)).getFile());
		Logger.getAnonymousLogger().log(Level.FINEST,"File is "+component.getName());
		uploadFile(agentId, component,FITTESTConstants.DEFAULT_FITTEST_AGENT_COMPONENT_DEPLOYMENT_BASEDIR+"/");
		
		DeployComponent deploy =  FITTESTSingleton.getObjectFactory().createDeployComponent();
		deploy.setTo(agentId);
		deploy.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
		deploy.setResource(component.getName());
		_registry.findService(IConnectionService.class).sendMessage(deploy);
	}
	
	private void uploadFile(String agentId, File file,String to) throws FITTESTException{
		Logger.getAnonymousLogger().log(Level.FINEST,"Uploading "+file+" to "+to+" on "+agentId);
		Upload upload = FITTESTSingleton.getObjectFactory().createUpload();
		upload.setTo(agentId);
		upload.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
		upload.setResource(to+file.getName());
		upload.setResourceSize(file.length());
		_registry.findService(IConnectionService.class).sendMessage(upload);
		_registry.findService(IFileTransferService.class).
		download(_registry.findService(IConnectionService.class).getConnection(upload.getTo()).getOutputStream(), file.toURI().toString());
	}
	
}
