package eu.fittest.common.agent;


import eu.fittest.common.core.connection.impl.ConnectionServiceImpl;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.impl.IdentityServiceImpl;
import eu.fittest.common.core.identity.spec.FITTESTEntity;
import eu.fittest.common.core.registry.ServiceRegistryImpl;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.services.compression.impl.CompressionServiceImpl;
import eu.fittest.common.services.filetransfer.impl.FileTransferServiceImpl;
import eu.fittest.common.services.forward.impl.ForwardServiceImpl;


public abstract class AbstractAgent {
    
    protected IServiceRegistry _serviceRegistry;


    
    public IServiceRegistry getServiceRegistry() {
        return _serviceRegistry;
    }

    
    protected AbstractAgent(FITTESTEntity identity) throws FITTESTException {
        _serviceRegistry = new ServiceRegistryImpl();
        _serviceRegistry.registerService(new IdentityServiceImpl(identity));
        _serviceRegistry.registerService(new ConnectionServiceImpl());
        _serviceRegistry.addServiceListener(_serviceRegistry.findService(IConnectionService.class));//connection listens for new services that are registered in order to manage their messages
        _serviceRegistry.registerService(new FileTransferServiceImpl());
        _serviceRegistry.registerService(new CompressionServiceImpl());
    	_serviceRegistry.registerService(new ForwardServiceImpl());
    }

}
