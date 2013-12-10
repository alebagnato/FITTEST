package eu.fittest.eclipse.component.junit.wizard;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.xml.Upload;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.component.appdescription.ite.services.properties.spec.AvailableProperties;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.model.environment.Host;

public abstract class JUnitInitializationTask{
	private SessionProperties _sessionProperties;
	private IServiceRegistry _registry;
	private IFolder _sessionFolder;
	private RequestPropertyListener _propertyListener;
	
	public JUnitInitializationTask(SessionProperties sessionProperties, IServiceRegistry registry, IFolder sessionFolder, RequestPropertyListener propertyListener) {
		_sessionProperties = sessionProperties;
		_registry = registry;
		_sessionFolder = sessionFolder;
		_propertyListener = propertyListener;
	}
	
	protected void uploadTestCases(Host h,String component) throws FITTESTException{
		try {
			Collection<File> testcases = ResourceUtils.collectFiles(_sessionProperties.getTempFolder(), "class");
			for(File tc: testcases){
					String packagePath = _sessionProperties.getTempFolder().toURI().relativize(tc.getParentFile().toURI()).getPath().toString();
					uploadFile(h, tc, component+"/"+_sessionFolder.getName()+"/"+packagePath, false, 0L);
			}
		} catch (CoreException e) {
			throw new FITTESTException(e.getMessage());  
		}
	}
	
	protected void uploadProfile(Host h,String component) throws IOException, FITTESTException{	
		if(_sessionProperties.getFirefoxProfile()!=null){
			uploadFile(h, _sessionProperties.getFirefoxProfile(), component+"/"+_sessionFolder.getName()+"/",true, _sessionProperties.getCRC());
		}
}
	
	protected void uploadTestProperties(Host h,String component) throws IOException, FITTESTException{
		File propertiesFile = _sessionFolder.getFile("test.properties").getLocation().toFile();
		
		Properties properties = new Properties();
		FileInputStream fis =new FileInputStream(propertiesFile); 
		properties.load(fis);
		fis.close();
				
		properties.setProperty(AvailableProperties.SELENIUM, _propertyListener.waitForValue(AvailableProperties.SELENIUM));
		FileOutputStream fos =new FileOutputStream(propertiesFile); 
		properties.store(fos, "updated with server url");
		fos.close();
				
		uploadFile(h, propertiesFile, component+"/"+_sessionFolder.getName()+"/",false, 0L);
	}
	
	protected void uploadFile(Host h, File file,String to, boolean inflate, long checksum) throws FITTESTException{
		Upload upload = FITTESTSingleton.getObjectFactory().createUpload();
		upload.setTo(h.getName());
		upload.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
		upload.setResource(to+file.getName());
		upload.setResourceSize(file.length());
		upload.setInflate(inflate);
		upload.setChecksum(checksum);
		_registry.findService(IConnectionService.class).sendMessage(upload);
		_registry.findService(IFileTransferService.class).
		download(_registry.findService(IConnectionService.class).getConnection(upload.getTo()).getOutputStream(), file.toURI().toString());
	}
	
}
