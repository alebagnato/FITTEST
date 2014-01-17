package eu.fittest.common.core.exception;

import eu.fittest.common.core.service.Connection;

public class FITTESTConnectionClosedException extends FITTESTException{
	private Connection _connection;
	public FITTESTConnectionClosedException(String msg, Connection c) {
		super(msg);
		_connection = c;
	}
	
	public Connection getConnection(){
		return _connection;
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

}
