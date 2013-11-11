package eu.fittest.component.phplogger;

import java.io.File;
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
import eu.fittest.component.common.AbstractComponent;
import eu.fittest.component.services.registration.spec.IComponentRegistrationService;

public class PHPLoggerComponent extends AbstractComponent implements FITTESTExceptionListener{
	static private final String SUT_CONF_FILE="phploggercomponent.conf";
	static private final String LOGGING_ENABLED="phploggercomponent.logging.enabled";
	static private final String LOGGING_DIR="phploggercomponent.logging.dir";
	static private final String SUT_CONF_FOLDER=".fittest";
	
	private Properties _properties;
	
	private String _sessionName;
	private File _loggingDir;
	private String _pathToSUTConfFolder;
	
	private String _applicationFolder;

	protected PHPLoggerComponent(int port) throws FITTESTException {
		super(PHPLoggerComponent.class.getSimpleName(), port);
		_properties = new Properties();
		 _pathToSUTConfFolder = null;
	}

	@Override
	public void register() throws FITTESTException {
		super.register();
		_connection.addFITTESTExceptionListener(this);
	}
	
	@Override
	public void registered() {
	}

	@Override
	public void initialize(List<Parameter> parameters) {
		Iterator<Parameter> it = parameters.iterator();
		_sessionName = null;
		
		while (it.hasNext()) {
			Parameter p = it.next();
			Logger.getAnonymousLogger().log(Level.FINEST, p.getName()+"="+p.getValue());
			if(p.getName().equals("session.name")){
				_sessionName = p.getValue();
			}
			if(p.getName().equals("application.folder")){
				_applicationFolder = p.getValue();
			}
		}
		Logger.getAnonymousLogger().log(Level.INFO,"session "+_sessionName+" starting");
		if(_sessionName!=null){
			String sessionFolderName = _serviceRegistry.findService(IComponentRegistrationService.class).getComponentDir()+"/"+_sessionName+"/";
			try {
				_loggingDir = new File(new URI(sessionFolderName));
				if(!_loggingDir.exists() && !_loggingDir.mkdirs()) throw new FITTESTException("Can't create folder "+_loggingDir.getAbsolutePath());
				_loggingDir.setWritable(true, false);//to allow www-data user to write into this folder
				_properties.setProperty(LOGGING_DIR, _loggingDir.getAbsolutePath());
				_properties.setProperty(LOGGING_ENABLED, Boolean.toString(false));
				_pathToSUTConfFolder=_applicationFolder+File.separator+SUT_CONF_FOLDER;
				save();
			} catch (URISyntaxException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			} catch (FITTESTException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			} catch (FileNotFoundException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
	}

	private void save() throws IOException{
		FileOutputStream fos =new FileOutputStream(new File(_pathToSUTConfFolder,SUT_CONF_FILE)); 
		_properties.store(fos, "session "+_sessionName);
		fos.close();
	}
	
	@Override
	public void start() {
		_properties.setProperty(LOGGING_ENABLED, Boolean.toString(true));
		try {
			save();
		} catch (FileNotFoundException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

	@Override
	public void stop() {
		_properties.setProperty(LOGGING_ENABLED, Boolean.toString(false));
		try {
			save();
		} catch (FileNotFoundException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

	@Override
	public void terminate() {
		_pathToSUTConfFolder = null;
		UploadResource ur = FITTESTSingleton.getObjectFactory().createUploadResource();
		ur.setFrom(_serviceRegistry.findService(IIdentityService.class).getMyIdentity());
		ur.setTo(_serviceRegistry.findService(IComponentRegistrationService.class).getAgentId());
		ur.setTowards(_serviceRegistry.findService(IComponentRegistrationService.class).getIteId());
		ur.setResource(_loggingDir.toURI().toString());
		try {
			_serviceRegistry.findService(IConnectionService.class).sendMessage(ur);
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}
	
	public void uncaughtException(Throwable t) {
		if(t instanceof FITTESTConnectionClosedException){
			Logger.getAnonymousLogger().log(Level.INFO, "Connection to agent closed");
			FITTESTSingleton.shutdown();
		}
	}

}
