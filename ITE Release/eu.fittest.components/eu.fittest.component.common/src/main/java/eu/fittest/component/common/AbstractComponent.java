package eu.fittest.component.common;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

import eu.fittest.common.core.connection.impl.ConnectionServiceImpl;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.impl.IdentityServiceImpl;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.registry.ServiceRegistryImpl;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.xml.Deregister;
import eu.fittest.common.core.xml.Register;
import eu.fittest.component.services.indirection.impl.IndirectionServiceImpl;
import eu.fittest.component.services.indirection.spec.IIndirectionService;
import eu.fittest.component.services.indirection.spec.IIndirectionServiceListener;
import eu.fittest.component.services.registration.impl.ComponentRegistrationServiceImpl;
import eu.fittest.component.services.registration.spec.IComponentRegistrationService;

public abstract class AbstractComponent implements IIndirectionServiceListener{
	protected Connection _connection;
	protected IServiceRegistry _serviceRegistry;
	protected String _componentName;
	private int _port;
	
	protected AbstractComponent(String componentName, int port) throws FITTESTException{
		System.setProperty(FITTESTConstants.FITTEST_AGENT_PORT, Integer.toString(port));
		_componentName = componentName;
		_port = port;
		_serviceRegistry = new ServiceRegistryImpl();
        _serviceRegistry.registerService(new ConnectionServiceImpl());
        _serviceRegistry.addServiceListener(_serviceRegistry.findService(IConnectionService.class));//connection listens for new services that are registered in order to manage their messages
        _serviceRegistry.registerService(new IdentityServiceImpl());
        _serviceRegistry.registerService(new ComponentRegistrationServiceImpl());
        _serviceRegistry.registerService(new IndirectionServiceImpl());
        _serviceRegistry.findService(IIndirectionService.class).addIndirectionServiceListener(this);
               		
	}
	
	public void register() throws FITTESTException{
		try {
			_connection = new Connection(new Socket("localhost", _port));
			_serviceRegistry.findService(IConnectionService.class).addConnection(_connection);
			Register register = FITTESTSingleton.getObjectFactory().createRegister();
			register.setFittestEntityName(_componentName);
			_connection.sendMessage(register);
		} catch (UnknownHostException e) {
			throw new FITTESTException(e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}
	}
	
	public void deregister() throws FITTESTException{
		Deregister deregister = FITTESTSingleton.getObjectFactory().createDeregister();
		deregister.setId(_serviceRegistry.findService(IIdentityService.class).getMyIdentity());
		deregister.setFrom(deregister.getId());
		deregister.setTo(_serviceRegistry.findService(IComponentRegistrationService.class).getIteId());
		_serviceRegistry.findService(IConnectionService.class).sendMessage(deregister);
	}
	
	@Override
	public void resourceAvailable(String comingFrom, String resource) {
		// TODO Auto-generated method stub	
	}
}
