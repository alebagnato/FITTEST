package eu.fittest.eclipse.component.contest.wizard;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceListener;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.common.core.xml.RequestProperties;
import eu.fittest.common.core.xml.RequestProperties.Property;
import eu.fittest.component.appdescription.ite.services.properties.spec.AvailableProperties;
import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesService;
import eu.fittest.component.appdescription.ite.services.properties.spec.PropertiesEvent;
import eu.fittest.eclipse.component.AbstractFITTESTComponentManager;
import eu.fittest.eclipse.component.ComponentInformation;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.contest.Activator;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.ComponentDeployment;
import eu.fittest.project.config.SUTTechnologyType;

public class ConTestComponentManager extends AbstractFITTESTComponentManager implements  IServiceListener<ServiceEvent<?>>{
	private ConTestPage _page=null;
	
	public static final ComponentInformation CONTEST_COMPONENT = new ComponentInformation("ConTestComponent",
			"/resources/components/eu.fittest.component.contest.zip", Activator.PLUGIN_ID);

	public ConTestComponentManager() {
		super(CONTEST_COMPONENT);
		_page = new ConTestPage();
	}
	
	@Override
	public IFITTESTComponentWizardPage[] getConfigurationPages() {
		return new IFITTESTComponentWizardPage[]{_page};
	}

	@Override
	public synchronized void initialize(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			String componentName = null;		
			if(deploymentCondition(host, CONTEST_COMPONENT.getName())){			
				waitForDeployment(host, CONTEST_COMPONENT.getName());
				componentName = CONTEST_COMPONENT.getName();
				_registry.findService(IPropertiesService.class).addServiceListener(this);
				requestProperty(_registry, host, AvailableProperties.EXECUTABLE_JAR);
			
				Initialize message = FITTESTSingleton.getObjectFactory().createInitialize();
				message.getParameter().addAll(_page.getInitializationParameters());
			
				Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
				p.setName("session.name");
				p.setValue(_sessionFolder.getName());
				message.getParameter().add(p);
			
				p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
				p.setName(AvailableProperties.EXECUTABLE_JAR);
				p.setValue(waitForValue(_registry, AvailableProperties.EXECUTABLE_JAR));
				message.getParameter().add(p);
			
				sendMessage(message, host, componentName);
			
				_registry.findService(IPropertiesService.class).removeServiceListener(this);//EXECUTABLE Jar property is available, no need to continue to listen
			}
			break;
		case RecordingSession:
			
			break;
		default:
			break;
		}
	}

	private void requestProperty(IServiceRegistry registry, Host h, String... propertyNames) throws FITTESTException{
		RequestProperties message = FITTESTSingleton.getObjectFactory().createRequestProperties();
		for(String name: propertyNames){
			Property p = FITTESTSingleton.getObjectFactory().createRequestPropertiesProperty();
			p.setName(name);
			message.getProperty().add(p);
		}
		message.setTo(ComponentDeployment.getInstance().findComponent(h.getName(), "AppDescriptionComponent"));
		message.setFrom(registry.findService(IIdentityService.class).getMyIdentity());
		registry.findService(IConnectionService.class).sendMessage(message);
	}
	
	synchronized private String waitForValue(IServiceRegistry registry, String property){
		IPropertiesService service = registry.findService(IPropertiesService.class);
		while(service.getProperty(property)==null){
			try {
				wait();
			} catch (InterruptedException e) {
			}
		}
		return service.getProperty(property);
	}

	public synchronized void incomingEvent(ServiceEvent<?> event) {
		super.incomingEvent(event);
		if(event instanceof PropertiesEvent && ((PropertiesEvent)event).getName().equals(AvailableProperties.EXECUTABLE_JAR)){
			notifyAll();
		}
	}
	
	protected boolean deploymentCondition(Host host, String componentName) {
		boolean deploy = false;
		if(_page.isEnabled()){
			if(componentName.equals(CONTEST_COMPONENT.getName())){
				deploy = (host.getType().equals(HUTType.SERVER) || host.getType().equals(HUTType.MIXED)) && host.getEnvironment().equals(HUTEnvironment.TEST);
			}
		}
		return deploy;
	}
	
	@Override
	public void start(Host host) throws FITTESTException {
		switch (_type) {
		case ReplayingSession:
			sendMessage(FITTESTSingleton.getObjectFactory().createStart(), host, CONTEST_COMPONENT.getName());
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
			sendMessage(FITTESTSingleton.getObjectFactory().createStop(), host, CONTEST_COMPONENT.getName());
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
			sendMessage(FITTESTSingleton.getObjectFactory().createTerminate(), host, CONTEST_COMPONENT.getName());
			break;
		case RecordingSession:
			
			break;
		default:
			break;
		}					
	}
	
}
