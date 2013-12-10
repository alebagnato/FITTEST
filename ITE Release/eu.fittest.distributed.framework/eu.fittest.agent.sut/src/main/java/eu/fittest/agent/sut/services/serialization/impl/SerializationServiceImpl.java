package eu.fittest.agent.sut.services.serialization.impl;

import java.io.BufferedInputStream;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap; // by urueda
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;


import eu.fittest.agent.sut.services.serialization.spec.ISerializationService;
import eu.fittest.agent.sut.services.serialization.spec.SerializationEvent;
import eu.fittest.agent.sut.services.serialization.spec.SerializationEventKind;
//import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.AbstractService;


public class SerializationServiceImpl extends AbstractService implements ISerializationService {	
	
	static final String EOF_LOG_CHUNK = "LOG_CHUNK_END"; // by urueda
	static HashMap<String,Boolean> waitSyncs = new HashMap<String,Boolean>(); // by urueda
	static ServerSocket _server; // permanent uploads server (by urueda)
	static Socket _socket; // permanent uploads socket (by urueda)
	static BufferedInputStream _bis; // permanent uploads stream (by urueda)

	private class SerializationJob implements Runnable{
		//private ServerSocket _server; // commented by urueda
		private BufferedOutputStream _resource;
		private String _resourceName;
		//private BufferedInputStream _bis; // commented by urueda
		//private Socket _socket; // commented by urueda
		private int eoflogchunk_idx = 0; // by urueda
		
		protected SerializationJob(ServerSocket server, String resource) throws FITTESTException{
			//_server = server; // commented by urueda
			_resourceName = resource;
			waitSyncs.put(resource,new Boolean(true));
			try {
				_resource = new BufferedOutputStream(new FileOutputStream(new File(new URI(resource))));
			} catch (FileNotFoundException e) {
				throw new FITTESTException(e.getMessage());
			} catch (URISyntaxException e) {
				throw new FITTESTException(e.getMessage());
			}
		}
		
		protected void close(){
			Boolean getR = waitSyncs.get(_resourceName); // by urueda
			if (getR!= null && getR) { // by urueda
				try {
					_bis.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		// by urueda
		private boolean eoflogchunk_checker(int c) throws IOException {
			if ((int)EOF_LOG_CHUNK.charAt(eoflogchunk_idx) == c) {
				eoflogchunk_idx++;
				if (eoflogchunk_idx == EOF_LOG_CHUNK.length()) {
					eoflogchunk_idx = 0;
					waitSyncs.put(_resourceName,new Boolean(false));
					_resource.close(); // does .flush()
					synchronized(EOF_LOG_CHUNK) {
						EOF_LOG_CHUNK.notifyAll();
					}
				}
				return false;
			}
			else {
				final int writeN = eoflogchunk_idx;
				while (eoflogchunk_idx > 0) {
					eoflogchunk_idx--;
					_resource.write(EOF_LOG_CHUNK.charAt(writeN - eoflogchunk_idx)); // write PARTIAL match with EOF_LOG_CHUNK 
				}
			}
			return true;
		}
		
		public void run() {
			try {
				//_socket = _server.accept(); // commented by urueda
				if (_socket == null || _socket.isClosed()) { // by urueda
					_socket = _server.accept(); // by urueda
					_bis =new BufferedInputStream(_socket.getInputStream());
				}
				int c;
	            while (waitSyncs.get(_resourceName) && (c = _bis.read()) != -1) {
	            	if (eoflogchunk_checker(c)) { // by urueda
	            		_resource.write(c);
	            	}
	            }
			} catch (IOException e) {
				ITELogger.log(Level.SEVERE,e.getMessage());
			}
			finally{
				terminate();					
			}			
		}
		
		public synchronized void terminate() {
			_ongoingSerializations.remove(_resourceName);
			try {
				Boolean getR = waitSyncs.get(_resourceName); // by urueda
				if (getR != null && getR) { // by urueda
					_bis.close();
					_socket.close();
					_server.close();
				}
				_resource.close(); // does .flush()
			} catch (IOException e) {
				ITELogger.log(Level.SEVERE,e.getMessage());
			}		
		}
	}
	
	
	private Map<String, SerializationJob> _ongoingSerializations;
	
	public SerializationServiceImpl() throws FITTESTException{
		_ongoingSerializations = new Hashtable<String, SerializationJob>();	
		_handlers.add(new StartSerializationMH(this));
		_handlers.add(new StopSerializationMH(this));
	}
	
	
	public String getName() {
		return ISerializationService.class.getName();
	}

	
	public String startSerialization(String resource) throws FITTESTException {				
		try {
			//ServerSocket server = new ServerSocket(0); // commented by urueda
			if ( _server == null || _server.isClosed()) { // by urueda
				_server = new ServerSocket(0);
			}
			String serverAddress = "localhost:"+_server.getLocalPort();
			SerializationJob job = new SerializationJob(_server,resource);
			_ongoingSerializations.put(resource, job);
			FITTESTSingleton.getThreadPool().execute(job);			
			ITELogger.log(Level.INFO,"start listening for serialization of "+resource+" on "+serverAddress);
			fireEvent(new SerializationEvent(this, resource, SerializationEventKind.start));
			
			return serverAddress;
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}

	}

	
	public void stopSerialization(String resource) throws FITTESTException {		
		// begin by urueda
		long waited = System.currentTimeMillis();
		while (waitSyncs.get(resource)) {
			synchronized(EOF_LOG_CHUNK) {
				try {
					EOF_LOG_CHUNK.wait(100); // wait for end of log chunk
				} catch (InterruptedException e) {
					ITELogger.log(Level.WARNING,"Interrupted waiting for end of log chunk");
				}
			}
			if (System.currentTimeMillis() - waited > 2000) { // 2 seconds
				ITELogger.log(Level.SEVERE, "Waited too much for end of log chunk");
				waitSyncs.put(resource, new Boolean(false));
			}
		}
		waitSyncs.remove(resource);
		// end by urueda
		SerializationJob job =	_ongoingSerializations.get(resource);//synchronized
		ITELogger.log(Level.INFO,"stop serialization of "+resource);
		fireEvent(new SerializationEvent(this, resource, SerializationEventKind.stop));
		if(job!=null) job.close();		
	}

}
