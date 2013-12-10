package eu.fittest.eclipse.component.optimizer;

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
import eu.fittest.eclipse.component.contest.wizard.ConTestComponentManager;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.component.junit.wizard.JUnitComponentManager;
import eu.fittest.eclipse.component.junit.wizard.JUnitInitializationTask;
import eu.fittest.eclipse.component.junit.wizard.JUnitPreparationTask;
import eu.fittest.eclipse.component.junit.wizard.JUnitSeleniumPage;
import eu.fittest.eclipse.component.junit.wizard.RequestPropertyListener;
import eu.fittest.eclipse.component.junit.wizard.SessionProperties;
import eu.fittest.eclipse.component.junit.wizard.TestCaseSelectionPage;
import eu.fittest.eclipse.component.junit.wizard.TestConfigPage;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.project.config.SUTTechnologyType;

public class OptimizationComponentManager extends AbstractFITTESTComponentManager implements IFITTESTComponentManager {
	private SessionProperties _sessionProperties;
	private OptimizerPage _optPage;
	private JUnitSeleniumPage _seleniumPage;
	private TestCaseSelectionPage _tcPage;
	private TestConfigPage _testconfigPage;
	private RequestPropertyListener _propertyListener;
	private Host _server;
	private Integer _initializationTask = 0;
	private Object _sync;
	
	public static final ComponentInformation OPTIMIZER_COMPONENT = new ComponentInformation("ConTestOptimizer", "/resources/components/eu.fittest.component.optimizer.zip", Activator.PLUGIN_ID);
	
	private class InitializationTaskForOptimizer extends JUnitInitializationTask implements Runnable{
		private Host _host;
		
		public InitializationTaskForOptimizer(Host host,SessionProperties sessionProperties, IServiceRegistry registry, IFolder sessionFolder, RequestPropertyListener propertyListener) {
			super(sessionProperties, registry, sessionFolder, propertyListener);
			_host = host;
		}
		
		@Override
		public void run() {
			try {
				String junitComponentInstance = waitForDeployment(_host, JUnitComponentManager.JUNIT_COMPONENT.getName());
			
				uploadProfile(_host, junitComponentInstance);
				uploadTestProperties(_host, junitComponentInstance);
				uploadTestCases(_host, junitComponentInstance);
				
				Initialize message = FITTESTSingleton.getObjectFactory().createInitialize();
				message.setTo(junitComponentInstance);
				message.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
				
				Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
				p.setName("session.name");
				p.setValue(_sessionFolder.getName());
				message.getParameter().add(p);
				
				p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
				p.setName("report.sendTo");
				Logger.getAnonymousLogger().log(Level.INFO, "Waiting for Optimizer Component");
				String optimizerComponent = waitForDeployment(_host, OPTIMIZER_COMPONENT.getName());
				Logger.getAnonymousLogger().log(Level.INFO, "Done Waiting for Optimizer Component");
				p.setValue(optimizerComponent);
				message.getParameter().add(p);
							
				message.getParameter().addAll(_seleniumPage.getInitializationParameters());
				
				_registry.findService(IConnectionService.class).sendMessage(message);
				
				if(deploymentCondition(_host, OPTIMIZER_COMPONENT.getName())){
					message = FITTESTSingleton.getObjectFactory().createInitialize();
					message.setTo(optimizerComponent);
					message.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
					
					p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
					p.setName("session.name");
					p.setValue(_sessionFolder.getName());
					message.getParameter().add(p);
					
					p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
					p.setName("junitcomponent.id");
					p.setValue(junitComponentInstance);
					message.getParameter().add(p);

					p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
					p.setName("contestcomponent.id");
					String contestId = waitForDeployment(_server, ConTestComponentManager.CONTEST_COMPONENT.getName());
					p.setValue(contestId);
					message.getParameter().add(p);
					
					p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
					p.setName(AvailableProperties.EXECUTABLE_JAR);
					p.setValue(_propertyListener.waitForValue(AvailableProperties.EXECUTABLE_JAR));
					message.getParameter().add(p);
					
					message.getParameter().addAll(_optPage.getInitializationParameters());
					
					_registry.findService(IConnectionService.class).sendMessage(message);
				}
				
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
	public void setServiceRegistry(IServiceRegistry registry) {
		super.setServiceRegistry(registry);
		_propertyListener = new RequestPropertyListener(_registry);
	}
	
	public OptimizationComponentManager(){
		super(new ComponentInformation[]{OPTIMIZER_COMPONENT, ConTestComponentManager.CONTEST_COMPONENT, JUnitComponentManager.JUNIT_COMPONENT});
		_optPage = new OptimizerPage();
		_seleniumPage = new JUnitSeleniumPage();
		_tcPage = new TestCaseSelectionPage();
		_testconfigPage = new TestConfigPage();
		_sync = new Object();
	}
	
	@Override
	public IFITTESTComponentWizardPage[] getConfigurationPages() {
		return new IFITTESTComponentWizardPage[]{_tcPage,_optPage,_seleniumPage,_testconfigPage};
	}
	
	@Override
	public void initialize(Host host) throws FITTESTException {		
		switch (_type) {
		case OptimizationSession:
			if(deploymentCondition(host, ConTestComponentManager.CONTEST_COMPONENT.getName())){
				//nothing
			}
			if(deploymentCondition(host, JUnitComponentManager.JUNIT_COMPONENT.getName())){
				synchronized (_sync) {
					_initializationTask++;	
				}
				FITTESTSingleton.getThreadPool().execute(new InitializationTaskForOptimizer(host,_sessionProperties,_registry,_sessionFolder,_propertyListener));//threaded so that the SELENIUM property can be requested  
			}
			if(deploymentCondition(host, AppDescriptionComponentManager.APPDESCRIPTION_COMPONENT.getName())){
				_server = host;
				_propertyListener.requestProperty(_server, AvailableProperties.SELENIUM,  AvailableProperties.EXECUTABLE_JAR);
			}
			break;
		default:
			break;
		}
	}

	@Override
	public void start(Host host) throws FITTESTException {
		if(deploymentCondition(host,OPTIMIZER_COMPONENT.getName())){
			_propertyListener.waitForValue(AvailableProperties.SELENIUM);
			synchronized (_sync) {
				while(_initializationTask>0){
					try {
						_sync.wait();
					} catch (InterruptedException e) {
					}
				}
			}
			sendMessage(FITTESTSingleton.getObjectFactory().createStart(), host,OPTIMIZER_COMPONENT.getName());		
		}
	}

	@Override
	public void stop(Host host) throws FITTESTException {
		if(deploymentCondition(host, OPTIMIZER_COMPONENT.getName())){
			sendMessage(FITTESTSingleton.getObjectFactory().createStop(), host, OPTIMIZER_COMPONENT.getName());
		}
	}

	@Override
	public void terminate(Host host) throws FITTESTException {
		if(deploymentCondition(host, OPTIMIZER_COMPONENT.getName())){
			sendMessage(FITTESTSingleton.getObjectFactory().createTerminate(), host, OPTIMIZER_COMPONENT.getName());
		}
	}

	@Override
	protected boolean deploymentCondition(Host host, String componentName) {
		boolean deploy = false;
		if(_optPage.isEnabled() && host.getEnvironment().equals(HUTEnvironment.TEST)){
			if(componentName.equals(OPTIMIZER_COMPONENT.getName())){
				deploy = host.getType().equals(HUTType.CLIENT) || host.getType().equals(HUTType.MIXED);
			}
			else if(componentName.equals(ConTestComponentManager.CONTEST_COMPONENT.getName())){
				deploy = host.getType().equals(HUTType.SERVER) || host.getType().equals(HUTType.MIXED);
			}
			else if(componentName.equals(JUnitComponentManager.JUNIT_COMPONENT.getName())){
				deploy = host.getType().equals(HUTType.CLIENT) || host.getType().equals(HUTType.MIXED);
			}
			else if(componentName.equals(AppDescriptionComponentManager.APPDESCRIPTION_COMPONENT.getName())){
				deploy = host.getType().equals(HUTType.SERVER) || host.getType().equals(HUTType.MIXED);
			}
		}
		return deploy;
	}
	
	private void createPropertiesFile(IFolder session) throws FileNotFoundException, IOException, CoreException {
		Properties properties = new Properties();
		
		properties.setProperty("application.name", _testconfigPage.applicationName);
		properties.setProperty("application.libs", _testconfigPage.libraries);
		properties.setProperty("selenium.speed", _testconfigPage.seleniumSpeed);
		
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
		_sessionProperties = new JUnitPreparationTask(_tcPage.getTestCases(), _registry, _seleniumPage.getPathToProfile()).prepare(session, type);
		try {
			createPropertiesFile(session);
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		} catch (CoreException e) {
			throw new FITTESTException(e.getMessage());
		}
	}
	

}
