package eu.fittest.common.core.connection.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;

import eu.fittest.common.services.registration.spec.RegistrationData;
import eu.fittest.common.util.ITELogger;

import eu.fittest.common.core.connection.spec.ConnectionServiceEvent;
import eu.fittest.common.core.connection.spec.ConnectionServiceEventKind;
import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTConnectionClosedException;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.exception.FITTESTExceptionListener;
import eu.fittest.common.core.registry.ServiceRegistryEvent;
import eu.fittest.common.core.server.spec.ServerEvent;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.IMessageHandler;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.Message;

public class ConnectionServiceImpl extends AbstractService implements IConnectionService, FITTESTExceptionListener {
    private List<Connection> _connections;

    private Map<String , Connection> _routingTable;

    private ArrayList<IMessageHandler> _registeredHandlers;
    
    public void registerMessageHandler(final IMessageHandler handler) {
    	_registeredHandlers.add(handler);
    	synchronized (_connections){
	        for(Connection c : _connections){
	            c.registerMessageHandler(handler);
	        }
    	}
    }
    
    public void unregisterMessageHandler(final IMessageHandler handler) {
    	_registeredHandlers.remove(handler);
    	synchronized (_connections){
	        for(Connection c : _connections){
	            c.unregisterMessageHandler(handler);
	        }
    	}
    }

    public void sendMessage(final Message message) throws FITTESTException {
    	if(message.getTo()==null) throw new FITTESTException("Message "+ message.getClass().getSimpleName()+" from "+message.getFrom()+" has no recipient defined");
    	Connection connection = null;
    	synchronized (_routingTable){
    		connection = _routingTable.get(message.getTo());
    	}
        if(connection!=null){
        	connection.sendMessage(message);
        } else {
        	broadcastMessage(null, message);
        }
    }
    
    public void broadcastMessage(Connection from, Message message) throws FITTESTException{
    	ITELogger.log(Level.INFO, "Trying to broadcast message " +message.getClass().getSimpleName()+" from "+message.getFrom() + " to " +message.getTo());
    	synchronized (_routingTable){
	    	for(Connection c: _routingTable.values()){
	    		if(c!=from){
	    			ITELogger.log(Level.INFO, "Broadcasting message " +message.getClass().getSimpleName()+" from "+message.getFrom() + " to " +message.getTo() +" through "+c.getRemoteAddress());
	    			c.sendMessage(message);
	    		}
	    	}
    	}
    }

    public void map(final String recipientId, final Connection connection) {
    	synchronized (_routingTable){
    		_routingTable.put(recipientId, connection);
    	}
    }

    public Connection getConnection(final String IP, final int port) {
        synchronized (_connections){
        	Connection[] connections = _connections.toArray(new Connection[_connections.size()]);
			for (Connection c : connections) {
				if(c.getRemoteAddress().equals(IP+":"+port)){
					return c;
				}
				
			}
        }
        return null;
    }

    public void addConnection(final Connection connection) {
    	
    	synchronized (_connections){
    		_connections.add(connection);
    	}
        connection.addFITTESTExceptionListener(this);
        for(IMessageHandler h: _registeredHandlers){
        	connection.registerMessageHandler(h);
        }
        FITTESTSingleton.getThreadPool().execute(connection);
        fireEvent(new ConnectionServiceEvent(this, connection, ConnectionServiceEventKind.add));
    }
    
    private void removeConnection(Connection connection){
    	fireEvent(new ConnectionServiceEvent(this,connection, ConnectionServiceEventKind.remove));
    	synchronized (_connections){
    		_connections.remove(connection);
    	}
    	synchronized (_routingTable){
	    	for(String s: getReachableId(connection)){
	    		_routingTable.remove(s);
	    	}
    	}
    	// Close the connection
    	try {
			connection.stop();
		} catch (FITTESTException e) {
			ITELogger.log(Level.WARNING, e.getMessage());
		}
    }

    
    public String getName() {
        return IConnectionService.class.getName();
    }

    
    public void incomingEvent(ServerEvent event) {
        try {
            addConnection(new Connection(event.getSocket()));
        } catch (FITTESTException e) {
            ITELogger.log(Level.SEVERE, e.getMessage());
        }
    }

    
    public void incomingEvent(ServiceEvent event) {
        if(event instanceof ServerEvent){
            incomingEvent((ServerEvent)event);
        }
        else if(event instanceof ServiceRegistryEvent){
        	incomingEvent((ServiceRegistryEvent)event);
        }
    }
    
    public void incomingEvent(ServiceRegistryEvent event) {
    	switch (event.getKind()) {
		case SERVICE_ADDED:
	        for(IMessageHandler h: event.getService().getHandlers()){
	        	registerMessageHandler(h);
	        }
			break;
		case SERVICE_REMOVED:
	        for(IMessageHandler h: event.getService().getHandlers()){
	        	unregisterMessageHandler(h);
	        }
			break;
		default:
			break;
		}
    }

    public ConnectionServiceImpl() {
        _connections =  new ArrayList<Connection> ();
        _routingTable = new HashMap<String, Connection>();
        _registeredHandlers = new ArrayList<IMessageHandler>();
        registerMessageHandler(new PolicyFileMH(this));
    }

	
	public void uncaughtException(Throwable t) {
		if(t instanceof FITTESTConnectionClosedException){
			FITTESTConnectionClosedException e = (FITTESTConnectionClosedException)t;
			Connection c = e.getConnection();
			removeConnection(c);
		}
	}

	
	public List<String> getReachableId(Connection connection) {
		synchronized (_routingTable) {
			List<String> reachable = new ArrayList<String>();
	    	for(Entry<String,Connection> entry : _routingTable.entrySet()){
	    		if(connection.equals(entry.getValue())) reachable.add(entry.getKey());
	    	}
			return reachable;
		}
	}

	
	public Connection getConnection(String id) {
		synchronized (_routingTable){
			return _routingTable.get(id);
		}
	}

	
	public void unmap(String id) {
		synchronized (_routingTable){
			_routingTable.remove(id);
		}
	}
	
	@Override
	public void stop(){
		synchronized (_connections){
			for(Connection c : _connections){
				try {
					c.stop();
				} catch (FITTESTException e) {
					ITELogger.log(Level.WARNING, e.getMessage());
				}
			}
		}
	}

}
