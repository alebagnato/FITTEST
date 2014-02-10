package eu.fittest.common.core.service;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.AbstractFITTESTExceptionObservable;
import eu.fittest.common.core.exception.FITTESTConnectionClosedException;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.exception.FITTESTServiceConfigurationException;
import eu.fittest.common.core.xml.Message;


public class Connection extends AbstractFITTESTExceptionObservable implements Runnable {
    
    private List<IMessageHandler> _handler;

    
    private XMLSocket _socket;

    JAXBContext jaxbContext;
    
    private Marshaller _marshaller;

    
    private Unmarshaller _unmarshaller;

    private String _address;

    private void safeMarshal(final Message message, StringWriter writer) throws FITTESTException { 
    	try {
			_marshaller.marshal(message,writer);
		} catch (Exception e) { // could not reuse the MArshaller instance
    		try {
    			synchronized(_marshaller){
    				if (_marshaller == null)
    					_marshaller = jaxbContext.createMarshaller(); // lets create a new instance and try again
    				_marshaller.marshal(message,writer);
    			}
			} catch (JAXBException e2) {
				ITELogger.log(Level.SEVERE, "Marshalling error for: " + message.toString());
				throw new FITTESTException(e2.getMessage());
			}
    	}
    }
    
    private Message safeUnmarshal(StringReader reader) throws JAXBException { 
    	Message message = null;
    	try {
			message = (Message) _unmarshaller.unmarshal(reader);
		} catch (Exception e) { 
    		try {
    			synchronized(_unmarshaller){
    				if (_unmarshaller == null)
    					_unmarshaller = jaxbContext.createUnmarshaller(); 
    				message = (Message) _unmarshaller.unmarshal(reader);
    			}
    		} catch (JAXBException e2) {
				ITELogger.log(Level.SEVERE, "Unmarshalling error: " + e2.getMessage());
				e2.printStackTrace();
				throw e2;
    		}
		}
		return message;
    }
    
    public synchronized void sendMessage(final Message message) throws FITTESTException {
            StringWriter writer = new StringWriter();
            safeMarshal(message,writer); 
            _socket.write(writer.toString());
            fireAfterSending(message);
    }

    
    private void receiveMessages() {
        try{
            while(!Thread.currentThread().isInterrupted() && !_socket.isClosed()){
                try {
                	Message m = safeUnmarshal(new StringReader(_socket.read())); // by urueda
                    try{
                        fireOnReception(m);
                    }catch(FITTESTServiceConfigurationException e){
                    	ITELogger.log(Level.SEVERE, e.getMessage());
                    } catch (FITTESTException e) {
                        ITELogger.log(Level.INFO, e.getMessage());
                    } 
                } catch (JAXBException e) {
                    ITELogger.log(Level.FINE, e.getMessage());
                    //should reply by an error message to the recipient
                }
            }
        } catch (IOException e) {
            ITELogger.log(Level.INFO, e.getMessage());
            //the connection is closed so we terminate the process
            fireException(new FITTESTConnectionClosedException(e.getMessage(), this));
        } catch (Exception e){
        	ITELogger.log(Level.SEVERE, "Connection message RECEIVE ERROR!");
        	e.printStackTrace();
        	Thread.dumpStack();
        }
    }

    
    public void registerMessageHandler(final IMessageHandler handler) {
        _handler.add(handler);
    }
    
    public void unregisterMessageHandler(final IMessageHandler handler) {
        _handler.remove(handler);
    }

    
    public String getRemoteAddress() {
        return _address;
    }

    
    public Connection(Socket socket) throws FITTESTException {
        try {
        	_address = socket.getInetAddress().getHostAddress()+":"+socket.getPort();
            _socket = new XMLSocket(socket);
        } catch (IOException e) {
            throw new FITTESTException(e.getMessage());
        }
        _handler = new ArrayList<IMessageHandler> ();
        try {
        	jaxbContext = JAXBContext.newInstance(FITTESTConstants.DEFAULT_FITTEST_JAXB_CONTEXT);
            _marshaller = jaxbContext.createMarshaller();
            _unmarshaller = jaxbContext.createUnmarshaller();
        } catch (JAXBException e) {
            throw new FITTESTException(e.getMessage());
        }
    }

    
    private void fireAfterSending(Message m) throws FITTESTException {
    	synchronized(_handler){
	        for (IMessageHandler handler : _handler.toArray(new IMessageHandler[_handler.size()])) { // by urueda
	            handler.afterSending(this, m);
	        }
    	}
        
    }

    
    private void fireOnReception(Message m) throws FITTESTException {
        for(IMessageHandler handler : _handler){
            handler.onReception(this, m);
        }
    }

    
    
    public void run() {
        receiveMessages();
    }

    
    public OutputStream getOutputStream(){
    	return _socket.getOutputStream();
    }

    
    public InputStream getInputStream() throws FITTESTException{
        try {
			return _socket.getInputStream();
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}
    }


	public void stop() throws FITTESTException {
		try {
			_socket.close();
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}
	}

}
