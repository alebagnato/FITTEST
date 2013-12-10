package eu.fittest.common.core.server.impl;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;

import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.server.spec.IServerService;
import eu.fittest.common.core.server.spec.IServerServiceListener;
import eu.fittest.common.core.server.spec.ServerEvent;
import eu.fittest.common.core.service.AbstractService;


public class ServerServiceImpl extends AbstractService implements IServerService, Runnable {
    
    private Thread _serverThread;

    
    private ServerSocket _server;


    
    public void start() {
        _serverThread.start();
    }

    
    public void stop() {
    	try {
            _serverThread.interrupt();
			_server.close();
		} catch (IOException e) {
			ITELogger.log(Level.INFO,e.getMessage());
		}
    }

    
    
    public void addServiceListener(IServerServiceListener listener) {
        super.addServiceListener(listener);
    }

    
    
    public void removeServiceListener(IServerServiceListener listener) {
        super.removeServiceListener(listener);
    }

    
    
    public String getName() {
        return IServerService.class.getName();
    }

    
    public ServerServiceImpl(boolean ssl) throws FITTESTException {
    	assert(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_START)!=null);
    	assert(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE)!=null);
    	assert(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_INCREMENT)!=null);
    	
    	Integer port = new Integer(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_START));
    	Integer range = new Integer(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE));
    	Integer increment = new Integer(System.getProperty(FITTESTConstants.FITTEST_SERVER_PORT_INCREMENT));
    	
        _server = scan(port, range, increment, ssl);
        _serverThread = new Thread(this);
    }
    
    public ServerServiceImpl() throws FITTESTException {
    	this(true);
    }

    
    
    public void run() {
        while(!Thread.currentThread().isInterrupted()){
            try {
                Socket socket = _server.accept();
                ITELogger.log(Level.INFO,"connection from "+socket.getRemoteSocketAddress());
                fireEvent(new ServerEvent(this, socket));
            } catch (IOException e) {
            	if(!Thread.currentThread().isInterrupted()){
            		ITELogger.log(Level.SEVERE,e.getMessage());
            	}
            }
        }
        ITELogger.log(Level.INFO,"Server stopped");
    }
    
    private ServerSocket scan(int start, int range, int increment, boolean ssl){
    	ServerSocket result = null;
    	int currentPort = start;
    	while(result==null && currentPort<start+range){
    		try {   
    			if(ssl){
	    			SSLServerSocket server = (SSLServerSocket) SSLServerSocketFactory.getDefault().createServerSocket(currentPort);
	    			server.setEnabledCipherSuites(FITTESTConstants.CIPHER_SUITE);
	    			result = server;
    			}
    			else{
    				result = new ServerSocket(currentPort);
    			}
    			ITELogger.log(Level.FINEST,"server socket on port:"+currentPort);
            } catch (IOException e) {
                ITELogger.log(Level.FINEST,e.getMessage()+" socket port:"+currentPort);
                currentPort+=increment;
            }
    	}
    	return result;
    }

	
	public Integer getServerPort() {
		return _server.getLocalPort();
	}

}
