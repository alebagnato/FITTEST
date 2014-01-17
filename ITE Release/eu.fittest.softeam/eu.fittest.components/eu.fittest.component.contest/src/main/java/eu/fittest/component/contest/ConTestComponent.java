package eu.fittest.component.contest;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTConnectionClosedException;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.exception.FITTESTExceptionListener;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.common.core.xml.UploadResource;
import eu.fittest.common.services.command.impl.CommandServiceImpl;
import eu.fittest.common.services.command.spec.ICommandService;
import eu.fittest.common.util.FileUtils;
import eu.fittest.component.common.AbstractComponent;
import eu.fittest.component.services.registration.spec.IComponentRegistrationService;

public class ConTestComponent extends AbstractComponent implements FITTESTExceptionListener{
	private static final File CONTEST_JAR = new File("ConTest.jar");//ConTest jar must be in component working dir
	private static final File CONTEST_LICENSE_FILE = new  File("contestLicense.dat");
	
	private String _sessionName;
	private String _jarToExecute;
	private String _uploadTowardsId = null;
	
	protected ConTestComponent(int port) throws FITTESTException {
		super(ConTestComponent.class.getSimpleName(),port);
		_serviceRegistry.registerService(new CommandServiceImpl());
		if(!CONTEST_JAR.exists()){
				throw new FITTESTException(CONTEST_JAR.getAbsolutePath()+" does not exist");
		}
		if(!CONTEST_LICENSE_FILE.exists()){
			throw new FITTESTException(CONTEST_LICENSE_FILE.getAbsolutePath()+" does not exist");
		}
	}

	@Override
	public void registered() {
		// TODO Auto-generated method stub

	}
	
	@Override
	public void register() throws FITTESTException {
		super.register();
		_connection.addFITTESTExceptionListener(this);
	}
	
	@Override
	public void uncaughtException(Throwable t) {
		if(t instanceof FITTESTConnectionClosedException){
			Logger.getAnonymousLogger().log(Level.INFO, "Connection to agent closed, exiting");
			stop();
			FITTESTSingleton.shutdown();
		}
	}

	@Override
	public void initialize(List<Parameter> parameters) {
		try {
			//edit king properties 
			Properties properties = new Properties();
	
			properties.load(new FileInputStream(IConTestComponentProperties.CONTEST_PROPERTY_FILE));
			// received session name, server name to execute and king properties target classes
			Iterator<Parameter> it = parameters.iterator();
			_sessionName = null;
			
			while (it.hasNext()) {
				Parameter p = it.next();
				if(p.getName().equals(IConTestComponentProperties.SESSION_NAME)){
					_sessionName = p.getValue();
				}
				else if(p.getName().equals(IConTestComponentProperties.JAVA_EXECUTABLE_JAR)){
					_jarToExecute = p.getValue();
				}
				else if(p.getName().equals(IConTestComponentProperties.RESULTS_UPLOAD_TOWARDS)){
					_uploadTowardsId = p.getValue();
				}
				else{
					properties.setProperty(p.getName(), p.getValue());
				}
			}
		
			if(properties.getProperty(IConTestComponentProperties.CONTEST_TARGETCLASSES) == null || _sessionName == null || _jarToExecute == null){
				throw new FITTESTException("ConTest component can not be configured: it requires a session name, target classes and a jar to execute");
			}
			properties.setProperty("licensePath", CONTEST_LICENSE_FILE.getAbsolutePath());
			
			//copy king properties to session folder
			String sessionFolderName = _serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+_sessionName+"/";
			File sessionFolderFile = new File(new URI(sessionFolderName));
			File contestFolder = new File(sessionFolderFile, "com_ibm_contest");
			if(sessionFolderFile.exists() && contestFolder.exists()){
				if(!FileUtils.deleteDir(contestFolder)){
					throw new FITTESTException("ConTest component can not be configured: the existing contest folder can't be deleted");
				}
			}
			File newProperties = new File(sessionFolderFile, IConTestComponentProperties.CONTEST_PROPERTY_FILE);
			newProperties.getParentFile().mkdirs();
			properties.store(new FileOutputStream(newProperties), "updated by ConTest FITTEST component");
		} catch (FileNotFoundException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		} catch (URISyntaxException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
		
	}

	@Override
	public void start() {
		// start contest
		try {
			_serviceRegistry.findService(ICommandService.class).execute("java -javaagent:"+CONTEST_JAR.getAbsolutePath()+" -jar "+_jarToExecute, null, null, new URI(_serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+"/"+_sessionName+"/"));
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		} catch (URISyntaxException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

	@Override
	public void stop() {
		try {
			_serviceRegistry.findService(ICommandService.class).killAllRunningTasks();
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

	@Override
	public void terminate() {
		//upload
		UploadResource upload = FITTESTSingleton.getObjectFactory().createUploadResource();
		upload.setFrom(_serviceRegistry.findService(IIdentityService.class).getMyIdentity());
		upload.setTo(_serviceRegistry.findService(IComponentRegistrationService.class).getAgentId());
		if(_uploadTowardsId==null){
			upload.setTowards(_serviceRegistry.findService(IComponentRegistrationService.class).getIteId());
		}
		else{
			upload.setTowards(_uploadTowardsId);
		}
		upload.setResource(_serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+_sessionName+"/com_ibm_contest/");
		try {
			_serviceRegistry.findService(IConnectionService.class).sendMessage(upload);
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

}
