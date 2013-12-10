package eu.fittest.eclipse.component.phplogger.wizard;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.eclipse.component.AbstractFITTESTComponentManager;
import eu.fittest.eclipse.component.ComponentInformation;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.phplogger.Activator;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.project.config.SUTTechnologyType;
import eu.fittest.project.config.TestProject;

public class PHPLoggerComponentManager extends AbstractFITTESTComponentManager {
	private PHPLoggerPage _page;
	
	private static final ComponentInformation PHP_COMPONENT = new ComponentInformation("PHPLoggerComponent", "resources/components/eu.fittest.component.phplogger.zip",Activator.PLUGIN_ID);
	
	public PHPLoggerComponentManager() {
		super(PHP_COMPONENT);
		_page = new PHPLoggerPage();
	}

	@Override
	public IFITTESTComponentWizardPage[] getConfigurationPages() {
		TestProject activeProjectConfig = eu.fittest.eclipse.gui.Activator.getDefault().getActiveProjectConfig();
		if (activeProjectConfig != null && _page!=null){
			_page.setApplicationFolder(activeProjectConfig.getGeneral().getServerFolder());
		}
		return new IFITTESTComponentWizardPage[]{_page};
	}
	
	@Override
	public void initialize(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			if(deploymentCondition(host, PHP_COMPONENT.getName())){			
				waitForDeployment(host, PHP_COMPONENT.getName());
				
				Initialize message = FITTESTSingleton.getObjectFactory().createInitialize();
				message.getParameter().addAll(_page.getInitializationParameters());
				
				Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
				p.setName("session.name");
				p.setValue(_sessionFolder.getName());
				message.getParameter().add(p);
				
				sendMessage(message, host, PHP_COMPONENT.getName());
			}
			break;
		default:
			break;
		}
	}

	@Override
	protected boolean deploymentCondition(Host host, String componentName) {
		// should check if the session is recording
		return getSUTTechOfSelectedProject().equals(SUTTechnologyType.PHP) && (host.getType().equals(HUTType.SERVER) || host.getType().equals(HUTType.MIXED));
//		return _page.isEnabled() && (host.getType().equals(HUTType.SERVER) || host.getType().equals(HUTType.MIXED));
	}
	
	@Override
	public void start(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			if(deploymentCondition(host, PHP_COMPONENT.getName())){
				sendMessage(FITTESTSingleton.getObjectFactory().createStart(), host, PHP_COMPONENT.getName());		
			}
			break;
		default:
			break;
		}							
	}
	
	@Override
	public void stop(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			if(deploymentCondition(host, PHP_COMPONENT.getName())){
				sendMessage(FITTESTSingleton.getObjectFactory().createStop(), host, PHP_COMPONENT.getName());		
			}
			break;
		default:
			break;
		}			
	}
	
	@Override
	public void terminate(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			break;
		case RecordingSession:
			if(deploymentCondition(host, PHP_COMPONENT.getName())){
				sendMessage(FITTESTSingleton.getObjectFactory().createTerminate(), host, PHP_COMPONENT.getName());		
			}
			break;
		default:
			break;
		}
	}

	@Override
	public SUTTechnologyType[] getSupportSUT() {
		SUTTechnologyType[] rets = {SUTTechnologyType.PHP};
		return rets;
	}
}
