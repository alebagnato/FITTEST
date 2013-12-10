package eu.fittest.agent.ite.core;



import eu.fittest.agent.ite.services.agentcommand.impl.AgentCommandServiceImpl;
import eu.fittest.agent.ite.services.forward.impl.ITEForwardServiceImpl;
import eu.fittest.agent.ite.services.httpserver.impl.HTTPServerServiceImpl;
import eu.fittest.agent.ite.services.httpserver.spec.IHTTPServerService;
import eu.fittest.agent.ite.services.registration.impl.RegistrationServiceImpl;
import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.agent.AbstractAgent;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.FITTESTITE;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.server.impl.ServerServiceImpl;
import eu.fittest.common.core.server.spec.IServerService;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;


public class ITEAgent extends AbstractAgent {

    
    public ITEAgent() throws FITTESTException {
        super(new FITTESTITE());
        _serviceRegistry.registerService(new ITEForwardServiceImpl());
        _serviceRegistry.registerService(new ServerServiceImpl());   
        _serviceRegistry.registerService(new RegistrationServiceImpl());
        _serviceRegistry.registerService(new HTTPServerServiceImpl());
        _serviceRegistry.registerService(new AgentCommandServiceImpl());
        
        configureServices();
        bindServices();
    }
    
    private void bindServices() {
        _serviceRegistry.findService(IServerService.class).addServiceListener(_serviceRegistry.findService(IConnectionService.class));
        _serviceRegistry.findService(IConnectionService.class).addServiceListener(_serviceRegistry.findService(IRegistrationService.class));
	}

	private void configureServices() throws FITTESTException{
    	_serviceRegistry.findService(IFileTransferService.class).
    		setBasePath(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)+
    				_serviceRegistry.findService(IIdentityService.class).getMyIdentity()+"/");
    }

    
    public void start() throws FITTESTException {
        _serviceRegistry.findService(IServerService.class).start();
        _serviceRegistry.findService(IHTTPServerService.class).start();
    }
    
    public void stop() throws FITTESTException {
        _serviceRegistry.findService(IServerService.class).stop();
        _serviceRegistry.findService(IHTTPServerService.class).stop();
    	
    }

}
