package eu.fittest.agent.sut.services.ite.impl;

import java.io.IOException;
import java.net.UnknownHostException;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;

import eu.fittest.common.util.ITELogger;

import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

import eu.fittest.agent.sut.services.ite.spec.IITERegistrationService;
import eu.fittest.agent.sut.services.ite.spec.ITERegistrationEvent;
import eu.fittest.agent.sut.services.ite.spec.ITERegistrationEventKind;
import eu.fittest.common.core.connection.spec.ConnectionServiceEvent;
import eu.fittest.common.core.connection.spec.ConnectionServiceEventKind;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.common.core.xml.RegisterAgent;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.services.registration.spec.RegistrationData;

public class ITERegistrationServiceImpl extends AbstractService implements IITERegistrationService {
	private String _iteId;
	private Timer _timer;
	private boolean _autoregister = false;
	private RegisterAgent _registrationMessage;
	private Autoregister _autoregistrationTask;
	private boolean _isRegistered = false;
	private boolean _isRegistering = false;
	
	private HUTEnvironment env;
	private HUTType type;
	private String description;
	
	public ITERegistrationServiceImpl(boolean autoregister){
		super();
		_autoregister = autoregister;
		_registrationMessage = null;
		_autoregistrationTask = null;
		_handlers.add(new RegisterAgentResponseMH(this));
	}

	public ITERegistrationServiceImpl(){
		this(false);
	}
	
	private class RenewLease extends TimerTask{
		
		public void run() {
			try {
				renewLease();
			} catch (FITTESTException e) {
				ITELogger.log(Level.SEVERE, e.getMessage());
			}			
		}
		
	}
	
	private class Autoregister extends TimerTask{
		
		public void run() {
			try {
				ITELogger.log(Level.INFO,"Trying to autoregister...");
				autoregister();
			} catch (FITTESTException e) {
				ITELogger.log(Level.INFO, "Autoregistration failed: " +e.getMessage());
			}			
		}		
	}
	
    public void renewLease() throws FITTESTException {
    	ITELogger.log(Level.INFO, "agent "+_registry.findService(IIdentityService.class).getMyIdentity()+" is renewing its lease");
		RegisterAgent register = FITTESTSingleton.getObjectFactory().createRegisterAgent();
		register.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
		register.setTo(_iteId);
		register.setDescription(description);
		register.setEnvironment(env);
		register.setType(type);
		_registry.findService(IConnectionService.class).sendMessage(register);
    }

    
    public synchronized void registeredAs(RegistrationData data) {
    	_registry.findService(IIdentityService.class).setMyIdentity(data.getEntity());
    	try {//for testing only
			configureServices();
		} catch (FITTESTException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
       _iteId = data.getIteid();
       _registry.findService(IConnectionService.class).map(_iteId,
    		   _registry.findService(IConnectionService.class).getConnection(System.getProperty(FITTESTConstants.FITTEST_ITE_ADDRESS),
    				   new Integer(System.getProperty(FITTESTConstants.FITTEST_ITE_PORT))));
       _timer.schedule(new RenewLease(), new Date((data.getLease()* 1000 - FITTESTConstants.RENEW_LEASE_BEFORE)));//renew lease xxx sec before end of lease
       
       _isRegistered = true;
       _isRegistering = false;
		
		if(_autoregistrationTask!=null){//message has been send, autoregistration can be cancelled;
			ITELogger.log(Level.INFO,"Autoregistration succeeded");
			_autoregistrationTask.cancel();
			_autoregistrationTask = null;
		}
		else{
			ITELogger.log(Level.INFO,"Registration succeeded");
		}
       fireEvent(new ITERegistrationEvent(this,data, ITERegistrationEventKind.Registered));
    }


	private void configureServices() throws FITTESTException{
    	_registry.findService(IFileTransferService.class).
    		setBasePath(System.getProperty(FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR)+
    				_registry.findService(IIdentityService.class).getMyIdentity()+"/");
    }
    
    
    public String getName() {
        return IITERegistrationService.class.getName();
    }

    
    public String getITEId() {
        return _iteId;
    }

	
	public synchronized void incomingEvent(ConnectionServiceEvent event) {
		if(_timer!=null){//service has been stopped or is not started yet
			if(event.getKind().equals(ConnectionServiceEventKind.remove) &&
					event.getConnection().equals(event.getSource().getConnection(_iteId))){//connection towards the ITE has been closed
				_isRegistered = false;
				
				if(_autoregister && _autoregistrationTask==null){
					_autoregistrationTask = new Autoregister();
					_timer.scheduleAtFixedRate(_autoregistrationTask, Long.parseLong(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_AUTOREGISTRATION_DELAY)),
							Long.parseLong(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_AUTOREGISTRATION_DELAY)));
				}
				
				fireEvent(new ITERegistrationEvent(this,null, ITERegistrationEventKind.Unregistered));
			}
		}
	}
	
	
	public synchronized void register(HUTEnvironment environment, HUTType type, String description) throws FITTESTException{
		
		this.env = environment;
		this.type = type;
		this.description = description;
		
		if(!_isRegistering){
			try {		
				//cache registration information
				_registrationMessage = FITTESTSingleton.getObjectFactory().createRegisterAgent();
				_registrationMessage.setEnvironment(environment);
				_registrationMessage.setType(type);
				_registrationMessage.setDescription(description);
				
				autoregister();
			} catch (NumberFormatException e) {
				throw new FITTESTException(e.getClass().getSimpleName()+": "+e.getMessage());
			}
		}
	}
	
	private synchronized void autoregister() throws FITTESTException{
		try {
			_isRegistering = true;
			//create connection to ITE
			SSLSocket socket = (SSLSocket) SSLSocketFactory.getDefault().createSocket(System.getProperty(FITTESTConstants.FITTEST_ITE_ADDRESS),
					new Integer(System.getProperty(FITTESTConstants.FITTEST_ITE_PORT)));
			socket.setEnabledCipherSuites(FITTESTConstants.CIPHER_SUITE);
			Connection connection = new Connection(socket);
			_registry.findService(IConnectionService.class).addConnection(connection);			
			
			//send registration to the ITE			
			_registrationMessage.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());//if entity has changed
			_registrationMessage.setFittestEntityName(_registrationMessage.getFrom());
			connection.sendMessage(_registrationMessage);
			
			
		} catch (NumberFormatException e) {
			throw new FITTESTException(e.getClass().getSimpleName()+": "+e.getMessage());
		} catch (UnknownHostException e) {
			throw new FITTESTException(e.getClass().getSimpleName()+": "+e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getClass().getSimpleName()+": "+e.getMessage() +":\nFITTEST ITE is probably not running");
		}
	}

	@Override
	public synchronized boolean isRegistered() {
		return _isRegistered;
	}

	@Override
	public synchronized void stop() {
		if(_timer!=null){
			_timer.cancel();
			_timer = null;
		}
	}

	@Override
	public synchronized void start() {
		if(_timer==null){
			_timer = new Timer(IITERegistrationService.class.getSimpleName()+" Autoregistration");
		}
	}

}
