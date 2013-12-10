package eu.fittest.common.core.connection.spec;

import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.ServiceEvent;

public class ConnectionServiceEvent extends ServiceEvent<ILocalConnectionService>{
	private ConnectionServiceEventKind _kind;
	private Connection _connection;
	
	public ConnectionServiceEvent(IConnectionService source, Connection connection, ConnectionServiceEventKind kind) {
		super(source);
		_connection = connection;
		_kind = kind;
	}
	
	public ConnectionServiceEventKind getKind(){
		return _kind;
	}

	public Connection getConnection() {		
		return _connection;
	}

}
