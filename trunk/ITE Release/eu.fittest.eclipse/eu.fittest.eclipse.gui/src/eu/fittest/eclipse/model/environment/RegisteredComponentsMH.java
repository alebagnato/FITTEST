package eu.fittest.eclipse.model.environment;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.FITTESTAgent;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.RegisteredComponents;
import eu.fittest.common.services.message.AbstractMessageHandler;
import eu.fittest.common.services.registration.spec.RegistrationData;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class RegisteredComponentsMH extends AbstractMessageHandler<IRegistrationService>{
	private List<IFITTESTComponentManager> _managers;
	
	protected RegisteredComponentsMH(IRegistrationService service, List<IFITTESTComponentManager> managers) {
		super(service);
		_managers = managers;
	}
	
	public synchronized void onReception(final Connection connection, final RegisteredComponents message) throws FITTESTException {
		RegistrationData data = FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(IRegistrationService.class).getAgentData(message.getFrom());
		FITTESTAgent agent = (FITTESTAgent)data.getEntity();
		Host tmp = new Host();
		tmp.setName(agent.getId());
		final int i = HostModel.getInstance().getHosts().indexOf(tmp);
		if(i!=-1){
			// Modified by Cu, move from inner thread to here so that its access is happened before the thread
			final Host h = HostModel.getInstance().getHosts().get(i);
			FITTESTSingleton.getThreadPool().execute(new Runnable() {			
				@Override
				public void run() {
					try {					
						for(IFITTESTComponentManager m:_managers){		
							m.deployOn(h);
						}
					} catch (FITTESTException e) {
						Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
					}
					
				}
			});
		}
		
    }

}
