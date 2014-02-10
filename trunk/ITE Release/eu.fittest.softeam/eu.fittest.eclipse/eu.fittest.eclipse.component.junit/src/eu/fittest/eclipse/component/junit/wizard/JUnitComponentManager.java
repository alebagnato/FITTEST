package eu.fittest.eclipse.component.junit.wizard;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.component.appdescription.ite.services.properties.spec.AvailableProperties;
import eu.fittest.eclipse.component.AbstractFITTESTComponentManager;
import eu.fittest.eclipse.component.ComponentInformation;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.appdescription.wizard.AppDescriptionComponentManager;
import eu.fittest.eclipse.component.junit.Activator;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.ComponentDeployment;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.project.config.SUTTechnologyType;

public class JUnitComponentManager extends AbstractFITTESTComponentManager{
	private SessionProperties _sessionProperties;
	private JUnitSeleniumPage _seleniumpage;
	private TestCaseSelectionPage _testcasepage;
	private TestConfigPage _testconfigpage;
	private RequestPropertyListener _propertyListener;
	private Object _sync;
	private int _initializationTask=0;
	
	public static final ComponentInformation JUNIT_COMPONENT = new ComponentInformation("JUnitComponent", "resources/components/eu.fittest.component.junit.zip", Activator.PLUGIN_ID);
	
	public JUnitComponentManager() {
		super(JUNIT_COMPONENT);
		_seleniumpage = new JUnitSeleniumPage();
		_testcasepage = new TestCaseSelectionPage();
		_testconfigpage = new TestConfigPage();
		_sync = new Object();
	}
	
	@Override
	public IFITTESTComponentWizardPage[] getConfigurationPages() {
		return new IFITTESTComponentWizardPage[]{_testcasepage, _testconfigpage,_seleniumpage};
	}
	
	@Override
	public void setServiceRegistry(IServiceRegistry registry) {
		super.setServiceRegistry(registry);
		_propertyListener = new RequestPropertyListener(_registry);
	}
	
	@Override
	protected boolean deploymentCondition(Host host, String componentName) {
		
		// should check if the session is replaying
		
		boolean deploy = false;
		if(host.getEnvironment().equals(HUTEnvironment.TEST)){
			if(componentName.equals(JUnitComponentManager.JUNIT_COMPONENT.getName())){
				deploy = host.getType().equals(HUTType.CLIENT) || host.getType().equals(HUTType.MIXED);
			}
			else if(componentName.equals(AppDescriptionComponentManager.APPDESCRIPTION_COMPONENT.getName())){
				deploy = host.getType().equals(HUTType.SERVER) || host.getType().equals(HUTType.MIXED);
			}
		}
		return deploy;
	}
	
	private class InitializationTaskForJunit extends JUnitInitializationTask implements Runnable{
		private Host _host;
		
		public InitializationTaskForJunit(Host host,SessionProperties sessionProperties, IServiceRegistry registry, IFolder sessionFolder, RequestPropertyListener propertyListener) {
			super(sessionProperties, registry, sessionFolder, propertyListener);
			_host = host;
		}
		
		@Override
		public void run() {
				try {
					waitForDeployment(_host, JUNIT_COMPONENT.getName());
					
					String componentInstance = ComponentDeployment.getInstance().findComponent(_host.getName(), JUNIT_COMPONENT.getName());
				
					uploadProfile(_host, componentInstance);
					uploadTestProperties(_host, componentInstance);
					uploadTestCases(_host, componentInstance);
					
					Initialize message = FITTESTSingleton.getObjectFactory().createInitialize();
					message.setTo(componentInstance);
					message.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
					Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
					p.setName("session.name");
					p.setValue(_sessionFolder.getName());
					message.getParameter().add(p);
					message.getParameter().addAll(_seleniumpage.getInitializationParameters());
					
					_registry.findService(IConnectionService.class).sendMessage(message);
				} catch (IOException e) {
					Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
				} catch (FITTESTException e) {
					Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
				}
				synchronized (_sync) {
					_initializationTask--;
					_sync.notify();						
				}
		}
		
	}
	
	@Override
	public void initialize(Host host) throws FITTESTException {		
		switch (_type) {
		case ReplayingSession:
			if(deploymentCondition(host, JUNIT_COMPONENT.getName())){
				synchronized (_sync) {
					_initializationTask++;	
				}
				FITTESTSingleton.getThreadPool().execute(new InitializationTaskForJunit(host,_sessionProperties,_registry,_sessionFolder,_propertyListener));//threaded so that the SELENIUM property can be requested 
			}
			if(deploymentCondition(host, AppDescriptionComponentManager.APPDESCRIPTION_COMPONENT.getName())){
				_propertyListener.requestProperty(host, AvailableProperties.SELENIUM);
			}
			break;
		case RecordingSession:
			
			break;
		default:
			break;
		}
	}
	
	@Override
	public void start(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			if(deploymentCondition(host,JUNIT_COMPONENT.getName())){
				_propertyListener.waitForValue(AvailableProperties.SELENIUM);
				synchronized (_sync) {
					while(_initializationTask>0){
						try {
							_sync.wait();
						} catch (InterruptedException e) {
						}
					}
				}
				sendMessage(FITTESTSingleton.getObjectFactory().createStart(), host,JUNIT_COMPONENT.getName());		
			}
			break;
		case RecordingSession:
			
			break;
		default:
			break;
		}							
	}
	
	@Override
	public void stop(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			sendMessage(FITTESTSingleton.getObjectFactory().createStop(), host,JUNIT_COMPONENT.getName());
			break;
		case RecordingSession:
			
			break;
		default:
			break;
		}			
	}
	
	@Override
	public void terminate(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			sendMessage(FITTESTSingleton.getObjectFactory().createTerminate(), host,JUNIT_COMPONENT.getName());
			break;
		case RecordingSession:
			
			break;
		default:
			break;
		}
	}
	
	private void createPropertiesFile(IFolder session) throws FileNotFoundException, IOException, CoreException {
		Properties properties = new Properties();
		
		properties.setProperty("application.name", _testconfigpage.applicationName);
		properties.setProperty("application.libs", _testconfigpage.libraries);
		properties.setProperty("selenium.speed", _testconfigpage.seleniumSpeed);
		properties.setProperty("selenium.initwait", _testconfigpage.loadingTime);
		
		IFile file = session.getFile("test.properties");
		FileOutputStream fos =new FileOutputStream(file.getLocation().toOSString()); 
		properties.store(fos, "Server under Test");
		fos.close();
		file.refreshLocal(IResource.DEPTH_ZERO, null);
		
	}
	
	@Override
	public void prepare(IFolder session, SessionType type) throws FITTESTException {
		synchronized (_sync) {
			_initializationTask = 0;	
		}
		super.prepare(session, type);
		switch (type) {
			case ReplayingSession:
				_sessionProperties = new JUnitPreparationTask(_testcasepage.getTestCases(), _registry, _seleniumpage.getPathToProfile()).prepare(session, type);
			try {
				createPropertiesFile(session);
			} catch (FileNotFoundException e) {
				throw new FITTESTException(e.getMessage());
			} catch (IOException e) {
				throw new FITTESTException(e.getMessage());
			} catch (CoreException e) {
				throw new FITTESTException(e.getMessage());
			}
				break;
			case RecordingSession:
				break;
			default:
				break;
		}
	}

	@Override
	public SUTTechnologyType[] getSupportSUT() {
		SUTTechnologyType[] rets = {SUTTechnologyType.FLASH, SUTTechnologyType.HTML};
		return rets;
	}

}
