package eu.fittest.common.services.message;

import java.lang.reflect.InvocationTargetException;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.IMessageHandler;
import eu.fittest.common.core.xml.Message;


public abstract class AbstractMessageHandler<T extends eu.fittest.common.core.service.IService> implements IMessageHandler {
    
    protected T _service;
	private IIdentityService _identity;

	public void setIdentityService(IIdentityService identity){
		_identity = identity;
	}

    
    public synchronized void onReception(final Connection connection, final Message message) throws FITTESTException {
    	if(_identity==null || _identity.getMyIdentity() == null || _identity.getMyIdentity().equals(message.getTo())){
	        try {
	            this.getClass().getMethod("onReception", Connection.class, message.getClass()).invoke(this, connection, message);
	        } catch (SecurityException e) {
	            throw new FITTESTException(e.getMessage());
	        } catch (NoSuchMethodException e) {
	            ITELogger.log(Level.FINEST,e.getMessage());
	        } catch (IllegalArgumentException e) {
	            throw new FITTESTException(e.getMessage());
	        } catch (IllegalAccessException e) {
	            throw new FITTESTException(e.getMessage());
	        } catch (InvocationTargetException e) {
	        	if(e.getCause() instanceof FITTESTException) throw (FITTESTException) e.getCause();
	        	else{
	        		StringBuilder builder = new StringBuilder();
	        		for(StackTraceElement element: e.getCause().getStackTrace()){
	        			builder.append(element.toString()+"\n");
	        		}
	        		throw new FITTESTException(e.getCause()+": "+e.getCause().getMessage()+"\n"+builder.toString());
	        	}
	        }
    	}
    }

    @SuppressWarnings("unused")
    
    private AbstractMessageHandler() {
    }

    
    protected AbstractMessageHandler(T service) {
        _service = service;
        _identity = null;
    } 
    
    public void afterSending(Connection connection, Message message) throws FITTESTException {
        try {
            this.getClass().getMethod("afterSending", Connection.class, message.getClass()).invoke(this, connection, message);
        } catch (SecurityException e) {
            throw new FITTESTException(e.getMessage());
        } catch (NoSuchMethodException e) {
            ITELogger.log(Level.FINEST,e.getMessage());
        } catch (IllegalArgumentException e) {
            throw new FITTESTException(e.getMessage());
        } catch (IllegalAccessException e) {
            throw new FITTESTException(e.getMessage());
        } catch (InvocationTargetException e) {
            throw new FITTESTException(e.getMessage());
        }
    }

    
    
    public T getService() {
        return _service;
    }

    protected static void reply(Connection connection, final Message request, final Message response) throws FITTESTException {
        response.setFrom(request.getTo());
        response.setTo(request.getFrom());
        connection.sendMessage(response);
    }

}
