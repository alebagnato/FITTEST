package eu.fittest.eclipse.component.appdescription.wizard;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.eclipse.component.AbstractFITTESTComponentManager;
import eu.fittest.eclipse.component.ComponentInformation;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.component.appdescription.Activator;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.project.config.SUTTechnologyType;

public class AppDescriptionComponentManager extends AbstractFITTESTComponentManager {
	
	public static final ComponentInformation APPDESCRIPTION_COMPONENT = new ComponentInformation("AppDescriptionComponent",
			"resources/components/eu.fittest.component.appdescription.zip", Activator.PLUGIN_ID);
	
	public AppDescriptionComponentManager() {
		super(APPDESCRIPTION_COMPONENT);
	}

	@Override
	public IFITTESTComponentWizardPage[] getConfigurationPages() {
		return new IFITTESTComponentWizardPage[0];
	}
	
	@Override
	public void initialize(Host host) throws FITTESTException {
		if(deploymentCondition(host, APPDESCRIPTION_COMPONENT.getName())){
			waitForDeployment(host, APPDESCRIPTION_COMPONENT.getName());
		}
	}

	@Override
	protected boolean deploymentCondition(Host host, String componentName) {
		return host.getType().equals(HUTType.SERVER)  || host.getType().equals(HUTType.MIXED);
	}
	
	@Override
	public void deployOn(Host host) throws FITTESTException {
		super.deployOn(host);
		if(deploymentCondition(host,APPDESCRIPTION_COMPONENT.getName())){
			waitForDeployment(host, APPDESCRIPTION_COMPONENT.getName());//make sure that the component is not deployed twice because this class is present for 2 extension points
		}
	}
	
	@Override
	public void start(Host host) throws FITTESTException {
		
	}
	
	@Override
	public void stop(Host host) throws FITTESTException {
				
	}
	
	@Override
	public void terminate(Host host) throws FITTESTException {
				
	}
	
	@Override
	public SUTTechnologyType[] getSupportSUT() {
		SUTTechnologyType[] rets = {SUTTechnologyType.FLASH};
		return rets;
	}

}
