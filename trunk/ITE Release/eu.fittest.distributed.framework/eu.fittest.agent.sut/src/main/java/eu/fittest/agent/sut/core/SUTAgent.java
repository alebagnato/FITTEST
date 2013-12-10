package eu.fittest.agent.sut.core;

import eu.fittest.agent.sut.services.component.impl.ComponentRegistrationServiceImpl;
import eu.fittest.agent.sut.services.component.spec.IComponentRegistrationService;
import eu.fittest.agent.sut.services.deployment.impl.DeploymentServiceImpl;
import eu.fittest.agent.sut.services.forward.impl.AgentForwardServiceImpl;
import eu.fittest.agent.sut.services.ite.impl.ITERegistrationServiceImpl;
import eu.fittest.agent.sut.services.ite.spec.IITERegistrationService;
import eu.fittest.agent.sut.services.serialization.impl.SerializationServiceImpl;
import eu.fittest.agent.sut.services.userdialog.impl.UserDialogServiceImpl;
import eu.fittest.common.agent.AbstractAgent;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.FITTESTAgent;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.server.impl.ServerServiceImpl;
import eu.fittest.common.core.server.spec.IServerService;
import eu.fittest.common.core.xml.Deregister;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.services.command.impl.CommandServiceImpl;
import eu.fittest.common.services.command.spec.ICommandService;


public class SUTAgent extends AbstractAgent{
	
	private HUTEnvironment env;
	private HUTType type;
	private String description;
	
	public SUTAgent() throws FITTESTException {
		super(new FITTESTAgent());
		FITTESTSingleton.setThreadPoolSize(20); // set the size for SUT agent
		_serviceRegistry.registerService(new AgentForwardServiceImpl());
		_serviceRegistry.registerService(new ServerServiceImpl(false));//disable SSL 
    	_serviceRegistry.registerService(new SerializationServiceImpl());
    	_serviceRegistry.registerService(new ITERegistrationServiceImpl(true));//enable autoregistration
    	_serviceRegistry.registerService(new ComponentRegistrationServiceImpl());
    	_serviceRegistry.registerService(new CommandServiceImpl());
    	_serviceRegistry.registerService(new DeploymentServiceImpl());
    	_serviceRegistry.registerService(new UserDialogServiceImpl());
    	_serviceRegistry.findService(IServerService.class).addServiceListener(_serviceRegistry.findService(IConnectionService.class));
    	_serviceRegistry.findService(IConnectionService.class).addServiceListener(_serviceRegistry.findService(IComponentRegistrationService.class));
    	_serviceRegistry.findService(IConnectionService.class).addServiceListener(_serviceRegistry.findService(IITERegistrationService.class));
    	_serviceRegistry.findService(IITERegistrationService.class).addServiceListener(_serviceRegistry.findService(IComponentRegistrationService.class));
	}
	
	public void register(HUTEnvironment environment, HUTType type, String description) throws FITTESTException{
		this.env = environment;
		this.type = type;
		this.description = description;
		_serviceRegistry.findService(IITERegistrationService.class).register(environment, type, description);
	}
	
	public void deregister() throws FITTESTException{
		if(_serviceRegistry.findService(IITERegistrationService.class).isRegistered()){
			Deregister deregister = FITTESTSingleton.getObjectFactory().createDeregister();
			deregister.setId(_serviceRegistry.findService(IIdentityService.class).getMyIdentity());
			deregister.setFrom(deregister.getId());
			deregister.setTo(_serviceRegistry.findService(IITERegistrationService.class).getITEId());
			_serviceRegistry.findService(IConnectionService.class).sendMessage(deregister);
		}
	}
	
	public void start() throws FITTESTException{
		_serviceRegistry.findService(IServerService.class).start();
		_serviceRegistry.findService(IITERegistrationService.class).start();
	}
	
	public void stop() throws FITTESTException{
		_serviceRegistry.findService(IConnectionService.class).stop();
		_serviceRegistry.findService(ICommandService.class).killAllRunningTasks();
		_serviceRegistry.findService(IServerService.class).stop();
		_serviceRegistry.findService(IITERegistrationService.class).stop();
		FITTESTSingleton.shutdown();
	}

	public HUTEnvironment getEnv() {
		return env;
	}

	public HUTType getType() {
		return type;
	}

	public String getDescription() {
		return description;
	}
	
	

}
